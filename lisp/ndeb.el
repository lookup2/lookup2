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

(defcustom ndeb-program-name (executable-find "eblook")
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

(defcustom ndeb-minimum-indent 1
  "指定した数字を越えた分だけ字下げ処理を行う。通常は0または1。"
  :type 'integer
  :group 'ndeb)

(defcustom ndeb-default-script-height 0.8
  "Default height of super/subscript letters.
1 or 0 mean height is not set."
  :type '(choice 
	  (list :tag "step" (const -) (integer :tag "count" :value 2))
	  (number :tag "scale factor" :value 0.8)
	  (function :tag "function")
	  (sexp :tag "other"))
  :group 'ndeb)

(defface ndeb-bold-face
  '((t (:weight bold)))
  "Face used to bold text."
  :group 'ndeb
  :group 'lookup-faces)

(defface ndeb-italic-face
  '((t (:slant italic)))
  "Face used to italic text."
  :group 'ndeb
  :group 'lookup-faces)

(defface ndeb-emphasis-face
  '((t (:slant italic :weight bold)))
  "Face used to emphasized text."
  :group 'ndeb
  :group 'lookup-faces)


;;;
;;; Interfaces
;;;

;; lookup entry command
(put 'ndeb :content   #'ndeb-entry-content)

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
     '((replace   lookup-arrange-replaces
                  ndeb-arrange-xbm
                  ndeb-arrange-bmp
                  ndeb-arrange-jpeg
                  ndeb-arrange-image-page
                  ndeb-arrange-wave
                  ndeb-arrange-mpeg
                  ndeb-arrange-scripts
                  ndeb-arrange-faces
                  ndeb-arrange-no-newline
                  ndeb-arrange-decode-entity
                  )
       (gaiji     lookup-arrange-gaijis)
       ;(media    lookup-arrange-media)              ; default
       (reference ndeb-arrange-auto-jump-reference
                  ndeb-arrange-paged-reference
                  ndeb-arrange-squeezed-references
                  lookup-arrange-references)
       (structure 
                  ndeb-arrange-prev-next ; to obtain prev/next entry.
                  ndeb-arrange-indent
                  lookup-arrange-structure
                  )
       (fill      lookup-arrange-fill-lines
                  ndeb-arrange-snd-autoplay
                  )
       ))

;; lookup content-arrangement functions and options
(put 'ndeb :gaiji-regexp  "<gaiji=\\([^>]*\\)>")
(put 'ndeb :gaiji     #'ndeb-dictionary-gaiji)

(put 'ndeb :media-pattern '())
(put 'ndeb :media     #'ndeb-dictionary-media)

(put 'ndeb :reference-pattern '("<reference>\\(→?\\(\\(.\\|\n\\)*?\\)\\)</reference=\\([^>]+\\)>" 1 2 4))

(put 'ndeb :charsets '(ascii japanese-jisx0208))

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

(defconst ndeb-entities-table
  '(("amp" . "&")
    ("lt" . "<")
    ("gt" . ">")))

(defconst ndeb-faces-table
  '(("italic" . ndeb-italic-face)
    ("bold" . ndeb-bold-face)
    ("em" . ndeb-emphasis-face)))
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

(defvar ndeb-support-escape-text nil
  "Positive numeric value means that eblook supports text escape.
Zero or negative value mean that feature is not supported.
Nil means it has not been checked yet.")

;; ndeb entry:
;;
;; CODE    - entry specific code (e.g. "2c00:340") by eblook `search' command
;; HEADING - given by eblook `search' command

(defun ndeb-new-entry (type code &optional heading)
  (lookup-new-entry type ndeb-current-dictionary code heading))

;;;
;;; macros
;;;

(put 'ndeb-with-agent 'lisp-indent-function 1)
(defmacro ndeb-with-agent (agent &rest body)
  "Switch eblook's context to AGENT and execute BODY."
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

(defun ndeb-process-open ()
  "eblookが起動していなければ起動する。"
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
      (set-process-query-on-exit-flag ndeb-process nil)
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
    (ndeb-process-require-set "decorate-mode" "on")

    ;; Check if eblook support escape text feature.
    (unless ndeb-support-escape-text
      (if  (string-match "^escape-text\t" (ndeb-process-require "show"))
	  (setq ndeb-support-escape-text 1)
	(setq ndeb-support-escape-text 0)))
    (when (> ndeb-support-escape-text 0)
      (ndeb-process-require-set "escape-text" "on"))))



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
  "Search EB DICTIONARY for QUERY."
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
            ;(when (re-search-forward "<more point=\\([0-9]*\\)>" nil t)
            ;  (setq entry (ndeb-new-entry 'dynamic "more"))
            ;  (lookup-put-property entry 'ndeb-query query)
            ;  (lookup-put-property entry 'ndeb-offset
            ;                       (string-to-int (match-string 1)))
            ;  (setq entries (cons entry entries)))
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
        (if lookup-max-text 
            (ndeb-process-require-set "max-text" lookup-max-text))
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
        (progn
          (lookup-put-property entry :preceding (lookup-get-link (point)))
          (replace-match "\n(前項目⇒"))
      (lookup-put-property entry :following (lookup-get-link (point)))
      (replace-match "(次項目⇒"))
    (if (re-search-forward "\\(</prev>\\|</next>\\)" nil t)
	(replace-match ")"))))

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

(defun ndeb-arrange-indent (entry)
  (while (re-search-forward "<ind=\\([0-9]\\)>" nil t)
    (let ((beg-beg (match-beginning 0))
	  (beg-end (match-end 0))
	  (level (- (string-to-number (match-string 1))
		    (or 
		     (lookup-dictionary-option dictionary ':minimum-indent t)
		     ndeb-minimum-indent)))
	  indent-end point)
      (delete-region beg-beg (point))
      (when (> level 0)
	(setq point (point))
	(setq indent-end
	      (or (and (re-search-forward "<ind=[0-9]>" nil t)
		       (match-beginning 0))
		  (point-max)))
	(set-left-margin point indent-end level)
	(goto-char point)))))

(defun ndeb-arrange-scripts (entry)
  (while (re-search-forward "<\\(su[bp]\\)>" nil t)
    (let ((beg-beg (match-beginning 0))
	  (tag (match-string 1)))
      (delete-region beg-beg (match-end 0))
      (if (and (re-search-forward (concat "<\\(/?\\)" tag ">") nil t)
	       (equal (match-string 1) "/"))
	  (let ((end-beg (match-beginning 0))
		(height (or (lookup-dictionary-option
			     dictionary ':script-height t)
			    ndeb-default-script-height)))
	    (goto-char end-beg)
	    (add-text-properties
	     beg-beg (point) `(display ,(delq nil
					      `((raise ,(if (equal tag "sub")
							    -0.3
							  0.3))
						,(if (or (eq height 0)
							 (eq height 1))
						     nil
						   `(height ,height))))))
	    (delete-region end-beg (match-end 0)))
	(goto-char beg-beg)))))

(defun ndeb-arrange-faces (entry)
  (while (re-search-forward "<\\(/?\\)em>" nil t)
    (if (equal (match-string 1) "/")
	(replace-match "</font>" t t)
      (replace-match "<font=em>" t t)))
  (goto-char (point-min))
  (while (re-search-forward "<font=\\([a-z]+\\)>" nil t)
    (let ((beg-beg (match-beginning 0))
	  (beg-end (match-end 0))
	  (class (match-string 1)))
      (if (and (re-search-forward
		"<\\(/?\\)font\\(=[a-z]+\\)?>" nil t)
	       (equal (match-string 1) "/"))
	  (let ((end-beg (match-beginning 0))
		(end-end (match-end 0)))
	    (add-text-properties beg-end end-beg
				 `(face ,(or (lookup-assoc-ref
					      'ndeb-faces-table class)
					     default)))
	    (delete-region end-beg end-end)
	    (delete-region beg-beg beg-end))
	(goto-char beg-beg)
	(delete-region beg-beg beg-end)))))

(defun ndeb-arrange-decode-entity (entry)
  (when (> ndeb-support-escape-text 0)
    (while (re-search-forward "&\\(amp\\|lt\\|gt\\);" nil t)
      (let* ((pos (match-beginning 0))
	     (properties (text-properties-at pos)))
	(replace-match (lookup-assoc-ref
			'ndeb-entities-table (match-string 1)) t t)
	(set-text-properties pos (1+ pos) properties)
	(goto-char (1+ pos))))))

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

;;; ndeb.el ends here
