;;; ndeb.el --- Another eblook interface
;; Copyright (C) 2006 Kazuhiro Ito <kzhr@d1.dion.ne.jp>

;; Author: Kazuhiro Ito <kzhr@d1.dion.ne.jp>

;; ndeb.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; ndeb.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:
;; ndeb.el is copied and modified from ndeb.el included in Lookup.

;;; Code:

(defconst ndeb-version "0.1")
(require 'lookup)
(require 'ndeb-binary)

;;;
;;; Customizable variables
;;;

(defgroup ndeb nil
  "Lookup ndeb interface."
  :group 'lookup-agents)

(defcustom ndeb-program-name "eblook"
  "*Program name of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-program-arguments '("-q" "-e" "euc-jp")
  "*A list of arguments for eblook."
  :type '(repeat (string :tag "option"))
  :group 'ndeb)

(defcustom ndeb-process-coding-system
  'euc-jp
  "*Coding system for eblook process."
  :type 'symbol
  :group 'ndeb)

(defcustom ndeb-prompt-string "eblook> "
  "*Prompt string of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-gaiji-size 16
  "デフォルトで使用する外字のサイズ。指定したサイズの外字が存在しない場合は指定値を越えない最大サイズを、それも存在しない場合は16ドットの外字を使用する。"
  :type '(choice :tag "size"
		 (const 16)
		 (const 24)
		 (const 30)
		 (const 48))
  :group 'ndeb)


;;;
;;; Interfaces
;;;

;; lookup entry command
(put 'ndeb :content   #'ndeb-entry-content)
(put 'ndeb :following #'ndeb-entry-following)
(put 'ndeb :preceding #'ndeb-entry-preceding)

;; lookup agent command
(put 'ndeb :list      #'ndeb-list)
(put 'ndeb :kill      #'ndeb-kill)

;; lookup dictionary command
(put 'ndeb :title     #'ndeb-dictionary-title)
(put 'ndeb :methods   #'ndeb-dictionary-methods)
(put 'ndeb :menu      #'ndeb-dictionary-menu)
(put 'ndeb :search    #'ndeb-dictionary-search)

;; arrangements

(put 'ndeb :arrange-table
     '((replace   ndeb-arrange-xbm
                  ndeb-arrange-bmp
                  ndeb-arrange-jpeg
                  ndeb-arrange-image-page
                  ndeb-arrange-wave
                  ndeb-arrange-mpeg
                  ndeb-arrange-prev-next
                  ndeb-arrange-snd-autoplay
                  lookup-arrange-replaces)
       (gaiji     ndeb-arrange-misc
                  lookup-arrange-gaijis)
       ;(media    lookup-arrange-media)              ; default
       (reference ndeb-arrange-auto-jump-reference
                  ndeb-arrange-paged-reference
                  ndeb-arrange-squeezed-references
                  lookup-arrange-references)
       (structure lookup-arrange-structure)
       (fill      ndeb-arrange-no-newline
                  lookup-arrange-fill-lines)
       ))

;; lookup content-arrangement functions and options
(put 'ndeb :gaiji-regexp  "<gaiji=\\([^>]*\\)>")
(put 'ndeb :gaiji     #'ndeb-dictionary-gaiji)

(put 'ndeb :media-pattern '())
(put 'ndeb :media     #'ndeb-dictionary-media)

(put 'ndeb :reference-pattern '("<reference>\\(→?\\(\\(.\\|\n\\)*?\\)\\)</reference=\\([^>]+\\)>" 1 2 4))

;(put'ndeb :font      nil)

;(put'ndeb :heading   nil)
;(put'ndeb :dynamic   nil)



;;;
;;; Dictionary Options
;;;

;; Options used by agent.
(put 'ndeb :stop-code     nil)


;;;;
;;;; Iternals
;;;;

;;;
;;; Internal constants
;;;

(defconst ndeb-method-table
  '((exact . "exact") (prefix . "word") (suffix . "endword") (wild . "wild")))

;;;
;;; Internal variables
;;;

(defvar ndeb-process nil
  "Process object for ndeb agents.")

(defvar ndeb-status nil
  "process stataus cache.")

(defvar ndeb-vars nil
  "process variables cache.")

(defvar ndeb-current-agent nil)

(defvar ndeb-current-dictionary nil)

;;;
;;; Macros
;;;

(put 'ndeb-with-agent 'lisp-indent-function 1)
(defmacro ndeb-with-agent (agent &rest body)
  `(let (book appendix)
     (ndeb-process-open)
     (unless (eq ,agent ndeb-current-agent)
       (setq book     (expand-file-name (lookup-agent-location ,agent))
             appendix (lookup-agent-option ,agent :appendix))
       (ndeb-process-require
        (concat "book " book " " appendix)
        (lambda (process)
          (if (search-forward "invalid book" nil t)
              (error "Invalid dictionary directory: %s" book))
          (if (search-forward "invalid appendix" nil t)
              (error "Invalid appendix directory: %s" book))))
       (setq ndeb-current-agent ,agent)
       (lookup-put-property ndeb-current-agent :ndeb-dict nil))
     ,@body))

(put 'ndeb-with-dictionary 'lisp-indent-function 1)
(defmacro ndeb-with-dictionary (dictionary &rest body)
  `(ndeb-with-agent (lookup-dictionary-agent ,dictionary)
     (let ((ndeb-current-dictionary ,dictionary))
       (unless (eq ,dictionary
                   (lookup-get-property ndeb-current-agent :ndeb-dict))
         (ndeb-process-require
             (concat "select " (lookup-dictionary-name ,dictionary)))
         (lookup-put-property ndeb-current-agent :ndeb-dict
                              ,dictionary))
       ,@body)))

;; send the COMMAND to `eblook' and process FILTER if given.
(defun ndeb-process-require (command &optional filter)
  (let ((lookup-process-output-separator-lines 0))
    (lookup-process-require 
     ndeb-process
     (concat command "\n")
;     (concat command "\nset prompt \""
;             ndeb-prompt-string "\"\nset prompt\n")
     (concat "^" ndeb-prompt-string)
     filter)))

;; set (or unset) VALUE of the VARiables of `eblook'.
;; it memorizes the value in cache.
(defun ndeb-process-require-set (var value)
  (unless (and (equal (ndeb-vars-get var) value) value)
    (ndeb-vars-set var value)
    (if value
	(ndeb-process-require (format "set %s \"%s\"" var value))
      (ndeb-process-require (format "unset %s" var)))))

;; If `eblook' process is not started yet, start it.
(defun ndeb-process-open ()
  (unless (and (processp ndeb-process)
	       (eq (process-status ndeb-process) 'run))
    (let ((buffer (or (lookup-open-process-buffer " *ndeb*")
		      (lookup-temp-buffer))))
      (setq ndeb-process (apply 'start-process "ndeb" buffer
				 ndeb-program-name ndeb-program-arguments)
	    ndeb-status nil
	    ndeb-vars nil)
      (set-process-coding-system ndeb-process
				 ndeb-process-coding-system
				 ndeb-process-coding-system)
      (process-kill-without-query ndeb-process)
      (with-current-buffer buffer
        (set-buffer-multibyte t)
	(catch 'started
	  (while (accept-process-output ndeb-process 10)
	    (save-excursion
	      (goto-char (point-min))
	      (when (search-forward ndeb-prompt-string nil t)
		(throw 'started t))))
	  (error "Failed start process")))
      (unless lookup-enable-debug
	(set-process-buffer ndeb-process nil)
	(kill-buffer buffer)))
    ;;(ndeb-process-require-set "prompt" "")
    (ndeb-process-require-set "prompt" ndeb-prompt-string)
    ))



;;;
;;; Interface Functions 
;;;

;; <:list>
(defun ndeb-list (agent)
  (ndeb-with-agent agent
   (ndeb-process-require "list"
     (lambda (process)
       (let (dicts)
         (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)" nil t)
           (setq dicts (cons (lookup-new-dictionary ndeb-current-agent
                                                    (match-string 1)) dicts)))
         (nreverse dicts))))))

;; <:kill>
(defun ndeb-kill (agent)
  (ndeb-agent-kill-process agent)
  (ndeb-binary-clear agent))

;; <:title>
(defun ndeb-dictionary-title (dictionary)
  (ndeb-dictionary-get-info dictionary "title"))

;; <:methods>
(defun ndeb-dictionary-methods (dictionary)
  (let ((string (ndeb-dictionary-get-info dictionary "search methods"))
        methods)
    (when string ; some books do not have search methods.
      (if (string-match "menu" string) 
          (setq methods (cons 'menu methods)))
      (if (string-match "endword" string)
          (setq methods (cons 'suffix methods)))
      (if (string-match "\\<word\\>" string)
          (setq methods (cons 'prefix methods)))
      (if (string-match "exactword" string)
          (setq methods (cons 'exact methods))))
    methods))

;; <:menu>
(defun ndeb-dictionary-menu (dictionary)
  (ndeb-with-dictionary dictionary
    (let ((string (ndeb-dictionary-get-info dictionary "search methods"))
          menu content image-menu image-content copyright copyright-content)
      (when string
        (when (string-match  " menu\\($\\| \\)" string)
          (setq menu    (list (ndeb-new-entry 'regular "menu" "[Menu]"))
                content (ndeb-process-require "menu"))
          (lookup-put-property menu 'ndeb-content content))
        (when (string-match  "image_menu" string)
          (setq image-menu (list (ndeb-new-entry 'regular "image_menu" "[Graphic menu]"))
                image-content (ndeb-process-require "image_menu"))
          (lookup-put-property image-menu 'ndeb-content image-content))
        (when (string-match "copyright" string)
          (setq copyright (list (ndeb-new-entry 'regular "copyright" "[Copyright]"))
                copyright-content (ndeb-process-require "copyright"))
          (lookup-put-property copyright 'ndeb-content copyright-content))
        )
    (nconc menu image-menu copyright))))

;; <:search>
(defun ndeb-dictionary-search (dictionary query)
  (ndeb-with-dictionary dictionary
    (let ((method (lookup-query-method query))
	  (string (lookup-query-string query))
	  (last (lookup-get-property ndeb-current-agent 'ndeb-method))
	  cmd)
      (cond
       ((eq method 'keyword)
	(let (qstring)
	  (setq qstring string)
	  (while (string-match "[ \t　]+" qstring)
	    (setq qstring (replace-match "=" nil t qstring)))
	  (setq cmd 
		(format "set search-method keyword\nsearch \"=%s\"\n"
			(ndeb-escape-query qstring)))
	  
	  (setq qstring string)
	  (while (string-match "[ \t　]+" qstring)
	    (setq qstring (replace-match "&" nil t qstring)))
	  (setq cmd 
		(concat cmd (format "set search-method cross\nsearch \"&%s\""
				    (ndeb-escape-query qstring))))
	  (lookup-put-property ndeb-current-agent 'ndeb-method "cross")))
       (t
	(when (eq method 'substring)
	  (setq method 'wild
		string (concat "*" string "*")))
	(unless (eq method last)
	  ;; 必要のあるときだけ search-method を設定する。ndeb-dict に同じ。
	  (ndeb-process-require-set "search-method"
			    (lookup-assq-ref 'ndeb-method-table method))
	  (lookup-put-property ndeb-current-agent 'ndeb-method method))
	(setq cmd (format "search \"%s\"" (ndeb-escape-query string)))))
      (ndeb-process-require cmd
        (lambda (process)
	  (let (code heading dupchk entry entries)
	    (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)\t\\(.*\\)" nil t)
	      (setq code (match-string 1) heading (match-string 2))
	      ;; 同じエントリがあるかチェックする。
	      ;; これがけっこうあるんだ・・
	      (unless (member (cons code heading) dupchk)
		(setq entries (cons (ndeb-new-entry 'regular code heading) entries))
		(setq dupchk (cons (cons code heading) dupchk))))
            (when (re-search-forward "<more point=\\([0-9]*\\)>" nil t)
              (setq entry (ndeb-new-entry 'dynamic "more"))
              (lookup-put-property entry 'ndeb-query query)
              (lookup-put-property entry 'ndeb-offset
                                   (string-to-int (match-string 1)))
              (setq entries (cons entry entries)))
	    (nreverse entries)))))))

;; <:gaiji>
(defun ndeb-dictionary-gaiji (dictionary code)
  (ndeb-with-dictionary dictionary
    (let ((height (lookup-dictionary-option dictionary :gaiji-size t))
          (tmp    (ndeb-dictionary-get-info dictionary "font sizes"))
          xbm)
      (when (string-match "[0-9]+$" tmp)
        (setq tmp (string-to-number (match-string 0 tmp)))
        (if (eq tmp ndeb-gaiji-size)
            (progn
              (setq height tmp)
              (goto-char (point-max)))
          (when (and (> ndeb-gaiji-size tmp)
                     (or (null height)
                         (< height tmp)))
            (setq height tmp))))
      (setq height (or height ndeb-gaiji-size))
      (lookup-dictionary-set-option dictionary
                                    :gaiji-size height)
      (ndeb-process-require-set "font" (number-to-string height)))
    (setq xbm (ndeb-process-require (concat "font " code)))
    (catch :done
      (when (string-match "default_width" xbm)
        (throw :done (list (vector 'xbm xbm))))
      ;; check later
      (when (equal (ndeb-process-require "show font") "16")
        (throw :done nil))
      (ndeb-process-require-set "font" "16")
      (setq xbm (ndeb-process-require (concat "font " code)))
      (when  (string-match "default_width" xbm)
        (list (vector 'xbm xbm))))))

;; <:content>
(defun ndeb-entry-content (entry)
  (or (lookup-get-property entry 'ndeb-content)
      (ndeb-with-dictionary (lookup-entry-dictionary entry)
        ;; processing `stop-code' if necessary.
        (let ((stop (lookup-dictionary-option dictionary ':stop-code t))
              (last (lookup-get-property ndeb-current-agent 'ndeb-stop)))
          (unless (eq stop last)
            (ndeb-process-require-set "stop-code" stop)
            (lookup-put-property ndeb-current-agent 'ndeb-stop stop)))
        (let ((code (lookup-entry-code entry)) return)
          (ndeb-process-require-set "decorate-mode" "on")
          (setq return
                (if (member code '("menu" "image_menu" "copyright"))
                    (ndeb-process-require code)
                  (ndeb-process-require (concat "content "
                                                (lookup-entry-code entry)))))
          (ndeb-process-require-set "decorate-mode" "off")
          return))))

;; <:preceding>
(defun ndeb-entry-preceding (entry)
  (let ((content (ndeb-entry-content entry)))
    (if (string-match "<prev>.*reference=\\([^>]+\\)>.*</prev>")
        (let ((ref (match-string 1)))))))

;; <:following>
(defun ndeb-entry-following (entry)
  (let ((content (ndeb-entry-content entry)))
    (if (string-match "<next>.*reference=\\([^>]+\\)>.*</next>")
        (let ((ref (match-string 1)))))))
	  
;;;
;;; Internal functions
;;;

(defun ndeb-agent-kill-process (agent)
  (let ((process (lookup-get-property agent 'ndeb-process)))
    (when process
      (if (eq (process-status process) 'run)
	  (process-send-string process "quit\n"))
      (lookup-process-kill process)
      (lookup-put-property agent 'ndeb-process nil))))


(defun ndeb-new-entry (type code &optional heading)
  (lookup-new-entry type ndeb-current-dictionary code heading))

(defun ndeb-dictionary-get-info (dictionary key)
  (let ((alist (lookup-get-property dictionary 'ndeb-alist)))
    (unless alist
      (setq alist
	    (ndeb-with-dictionary dictionary
	      (ndeb-process-require "subinfo"
		(lambda (process)
		  (let (alist)
		    (while (re-search-forward "^ \\([^:]+\\): \\(.*\\)" nil t)
		      (setq alist
			    (acons (match-string 1) (match-string 2) alist)))
		    alist)))))
      (lookup-put-property dictionary 'ndeb-alist alist))
    (lookup-assoc-get alist key)))

(defun ndeb-escape-query (string)
  (let ((start 0))
    (while (string-match "\\\\" string start)
      (setq string (replace-match "\\\\" t t string)
	    start (1+ (match-end 0)))))
  string)

;;; Arrange fnctions

(defun ndeb-arrange-squeezed-references (entry)
  (if (lookup-dictionary-option
       (lookup-entry-dictionary entry) ':squeezed nil)
      (while (search-forward-regexp "→□\\(#0001\\|<gaiji:z0001>\\)?" nil t)
	(replace-match ""))))

(defun ndeb-arrange-no-newline (entry)
  (while (search-forward "<no-newline>" nil t)
    (let ((beg-beg (match-beginning 0))
	  (beg-end (match-end 0)))
      (if (and (re-search-forward "<\\(/?\\)no-newline>" nil t)
	       (equal (match-string 1) "/"))
	  (let ((end-beg (match-beginning 0)))
	    (goto-char end-beg)
	    (skip-chars-backward "\t " beg-end)
	    (when (> (point) (point-min))
	      (backward-char))
	    (when (< beg-end (point))
	      (add-text-properties beg-end (point) '(read-only t)))
	    (delete-region end-beg (match-end 0))
	    (delete-region beg-beg beg-end))
	(goto-char beg-beg)
	(delete-region beg-beg beg-end)))))

(defun ndeb-arrange-prev-next (entry)
  (while (re-search-forward "\\(<prev>\\|<next>\\)" nil t)
    (if (equal (match-string 0) "<prev>")
	(replace-match "\n(前項目⇒")
      (replace-match "(次項目⇒"))
    (if (re-search-forward "\\(</prev>\\|</next>\\)" nil t)
	(replace-match ")"))))

(defun ndeb-arrange-misc (entry)
  (let ((start (point)) obj char)
    (while (re-search-forward "<sup>\\(.+?\\)</sup>" nil t)
      (save-match-data
        (setq obj (japanese-hankaku (match-string 1)))
        (dotimes (i (length obj))
          (setq char (lookup-superscript-character (aref obj i)))
          (if char (aset obj i char))))
      (replace-match obj))
    (goto-char start)
    (while (re-search-forward "<sub>\\(.+?\\)</sub>" nil t)
      (save-match-data
        (setq obj (japanese-hankaku (match-string 1)))
        (dotimes (i (length obj))
          (setq char (lookup-subscript-character (aref obj i)))
          (if char (aset obj i char))))
      (replace-match obj))
    (goto-char start)
    (while (re-search-forward "<em>\\(.+?\\)</em>" nil t)
      (save-match-data
        (setq obj (match-string 1))
        (put-text-property 0 (length em) 'face 'bold em))
      (replace-match em))))

(defun ndeb-arrange-auto-jump-reference (entry)
  (ndeb-with-dictionary (lookup-entry-dictionary entry)
    (when (re-search-forward "<auto-jump-reference></auto-jump-reference=\\([0-9]+:[0-9]+\\)>" nil t)
      (let ((code (match-string 1)))
        (delete-region (point-min) (point-max))
        (insert
         (if (eq (lookup-agent-class (lookup-dictionary-agent dictionary)) 'ndeb)
             (ndeb-entry-content (ndeb-new-entry 'regular code nil))
           (ndeb-entry-content
            (lookup-new-entry 'regular dictionary code nil))))))))

(defun ndeb-arrange-paged-reference (entry)
  (while (re-search-forward "<paged-reference=\\([0-9]+:[0-9]+\\)>" nil t)
    (let ((pos (match-string 1))
	  (start (match-beginning 0))
	  (end (match-end 0)))
      (condition-case nil
	  (progn
	    (search-forward "</paged-reference>")
	    (replace-match (format "</reference=%s>" pos))
	    (delete-region start end)
	    (goto-char start)
	    (insert "<reference>"))
	(error nil)))))

;;; Get/Set functions

(defun ndeb-status-get (key)
  (lookup-assq-ref 'ndeb-status key))

(defun ndeb-status-set (key value)
  (setq ndeb-status
	(lookup-assq-set 'ndeb-status key value)))

(defun ndeb-vars-get (var)
  (lookup-assoc-ref 'ndeb-vars var))

(defun ndeb-vars-set (var value)
  (setq ndeb-vars
	(lookup-assoc-set 'ndeb-vars var value)))

(provide 'ndeb)

;;; ndsrd.el ends here
