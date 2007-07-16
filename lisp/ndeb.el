;;; ndeb.el --- Lookup eblook interface
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndeb.el,v 1.6 2007/07/16 04:06:03 kawabata Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

(defconst ndeb-version "1.2")

;;;
;:: Customizable variables
;;;

(defgroup ndeb nil
  "Lookup eblook interface."
  :group 'lookup-agents)

(defcustom ndeb-program-name "eblook"
  "*Program name of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-program-arguments '("-q" "-e" "euc-jp")
  "*A list of arguments for eblook."
  :type '(repeat (string :tag "option"))
  :group 'ndeb)

(defcustom ndeb-prompt-string "eblook> "
  "*Prompt string of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-process-coding-system
  'euc-jp
  "*Coding system for eblook process."
  :type 'symbol
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

;; options
(put 'ndeb :gaiji-regexp "<gaiji=\\([^>]*\\)>")
(put 'ndeb :reference-pattern '("<reference>\\(→?\\(\\(.\\|\n\\)*?\\)\\)</reference=\\([^>]+\\)>" 1 2 4))
(put 'ndeb :arrange-table
     '((replace   ndeb-arrange-auto-jump-reference
                  ndeb-arrange-xbm
                  ndeb-arrange-bmp
                  ndeb-arrange-jpeg
                  ndeb-arrange-image-page
                  ndeb-arrange-wave
                  ndeb-arrange-mpeg
                  ndeb-arrange-ignore
                  ndeb-arrange-prev-next
                  ndeb-arrange-paged-reference)
       (gaiji     lookup-arrange-gaijis)
       (reference lookup-arrange-references ndeb-arrange-squeezed-references)
       (structure lookup-arrange-structure) ;; lookup-arrange-default-headings
       (fill      ndeb-arrange-no-newline
                  ndeb-arrange-fill-lines
                  ndeb-arrange-snd-autoplay)))

;;(put 'ndeb :adjusts
;;     '(lookup-adjust-check-references
;;       lookup-adjust-show-gaijis
;;       lookup-adjust-goto-min))

;; agent-command
(put 'ndeb :list      #'ndeb-list)
(put 'ndeb :kill      #'ndeb-kill)

;; dictionary-command
(put 'ndeb :title     #'ndeb-dictionary-title)
(put 'ndeb :methods   #'ndeb-dictionary-methods)
(put 'ndeb :menu      #'ndeb-dictionary-menu)
;(put'ndeb :copyright #'ndeb-dictionary-copyright)
(put 'ndeb :search    #'ndeb-dictionary-search)
(put 'ndeb :gaiji     #'ndeb-dictionary-gaiji)
;(put 'ndeb :font      ??)

;; entry-command
(put 'ndeb :content   #'ndeb-entry-content)
;(put 'ndeb :heading   nil)
;(put 'ndeb :dynamic   ??)


;;;
;:: Internal varialbes
;;;

(put 'ndeb :ignore-regexp "\\(</?su[pb]>\\|</?em>\\)")
;;(put 'ndeb :headings '(lookup-arrange-gaijis))

(defvar ndeb-current-agent nil)
(defvar ndeb-current-dictionary nil)
(defvar ndeb-current-process nil)

(defconst ndeb-method-table
  '((exact . "exact") (prefix . "word") (suffix . "endword") (wild . "wild")))

(defconst ndeb-gaiji-default-size 16)

;;;
;:: types
;;;

;; ndeb agent:
;;
;;   (ndeb DIRECTORY :appendix APPENDIX)
;;
;; DIRECTORY - dictionary directory
;; APPENDIX  - appendix directory
;;
;; [property]
;; ndeb-process - eblook process related with agent
;; ndeb-dict    - last used dictionary
;; ndeb-method  - last used value of search-method
;; ndeb-stop    - last used value of stop-code

(defun ndeb-agent-directory (agent)
  (let ((dir (lookup-agent-location agent)))
    (if dir 
	(if (string-match "^ebnet://" dir)
	    dir
	  (expand-file-name dir))
      (error "You should specify a dictionary directory"))))

(defun ndeb-agent-appendix (agent)
  (let ((dir (lookup-agent-option agent :appendix)))
    (if dir
	(if (string-match "^ebnet://" dir)
	    dir
	  (expand-file-name dir)))))

(defun ndeb-agent-coding (agent)
  (or (lookup-agent-option agent :coding)
      ndeb-process-coding-system))

(require 'ndeb-binary)

(defun ndeb-arrange-squeezed-references (entry)
  (if (lookup-dictionary-option
       (lookup-entry-dictionary entry) ':squeezed nil)
      (while (search-forward-regexp "→□\\(#0001\\|<gaiji:z0001>\\)?" nil t)
	(replace-match ""))))

;; ndeb dictionary:
;;
;; CODE - same as NAME below
;; NAME - given by eblook `list' command
;; 
;; [option]
;; :coding    - process coding system
;; :stop-code - stop-code used by eblook

;;(defun ndeb-new-dictionary (name title) * no longer used, /kawabata
;;  (lookup-new-dictionary ndeb-current-agent name name title))

(defun ndeb-dictionary-coding (dictionary)
  (or (lookup-dictionary-option dictionary ':coding t)
      ndeb-process-coding-system))

(defun ndeb-dictionary-stopcode (dictionary)
  (lookup-dictionary-option dictionary ':stop-code t))

;; ndeb entry:
;;
;; CODE    - entry specific code (e.g. "2c00:340") by eblook `search' command
;; HEADING - given by eblook `search' command

(defun ndeb-new-entry (type code &optional heading)
  (lookup-new-entry type ndeb-current-dictionary code heading))

;;;
;:: macros
;;;

(put 'ndeb-with-agent 'lisp-indent-function 1)
(defmacro ndeb-with-agent (agent &rest body)
  `(let ((ndeb-current-agent ,agent)
	 (ndeb-current-process (ndeb-agent-process ,agent)))
     ,@body))

(put 'ndeb-with-dictionary 'lisp-indent-function 1)
(defmacro ndeb-with-dictionary (dictionary &rest body)
  `(let ((dictionary ,dictionary))
     (ndeb-with-agent (lookup-dictionary-agent dictionary)
       (let ((ndeb-current-dictionary dictionary))
	 (unless (eq dictionary
		     (lookup-get-property ndeb-current-agent 'ndeb-dict))
	   (ndeb-process-require
	    (concat "select " (lookup-dictionary-name dictionary)))
	   (lookup-put-property ndeb-current-agent 'ndeb-dict
				dictionary)
	   (let ((code (ndeb-dictionary-coding dictionary)))
	     (when code
	       (set-process-coding-system ndeb-current-process code code))))
	 ,@body))))

(defun ndeb-agent-process (agent)
  (let ((process (lookup-get-property agent 'ndeb-process)))
    (unless (and process (eq (process-status process) 'run))
      (if process (lookup-process-kill process))
      (setq process (ndeb-process-open (ndeb-agent-directory agent)
				       (ndeb-agent-appendix agent)))
      ;; 最初に辞書一覧を得るのに文字コードの設定が必要。
      (let ((coding (ndeb-agent-coding agent)))
	(when coding
	  (set-process-coding-system process coding coding)))
      ;; コマンドの実行毎に行なう必要のある処理。
      (let ((ndeb-current-process process))
	(if lookup-max-hits (ndeb-require-set "max-hits" lookup-max-hits))
	(if lookup-max-text (ndeb-require-set "max-text" lookup-max-text)))
      (lookup-put-property agent 'ndeb-process process)
      (lookup-put-property agent 'ndeb-dict nil)
      (lookup-put-property agent 'ndeb-method nil)
      (lookup-put-property agent 'ndeb-stop nil))
    process))

(defun ndeb-agent-kill-process (agent)
  (let ((process (lookup-get-property agent 'ndeb-process)))
    (when process
      (if (eq (process-status process) 'run)
	  (process-send-string process "quit\n"))
      (lookup-process-kill process)
      (lookup-put-property agent 'ndeb-process nil))))



;:: Interface functions

(defun ndeb-list (agent)
  (ndeb-with-agent agent
    (ndeb-process-require "list"
      (lambda (process)
	(let (dicts)
	  (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)" nil t)
	    (setq dicts (cons (lookup-new-dictionary ndeb-current-agent
						     (match-string 1)) dicts)))
	  (nreverse dicts))))))

(defun ndeb-dictionary-title (dictionary)
  (ndeb-dictionary-get-info dictionary "title"))

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

(defun ndeb-kill (agent)
  (ndeb-agent-kill-process agent)
  (ndeb-binary-clear agent))


;; lookup 1.99
;(defun ndeb-dictionary-menu (dictionary)
;  (ndeb-with-dictionary dictionary
;    (let ((menu (ndeb-new-entry 'regular "menu" "[menu]"))
;	  (content (ndeb-process-require "menu")))
;      (lookup-put-property menu 'ndeb-content content)
;      menu)))

(defun ndeb-dictionary-menu (dictionary)
  (ndeb-with-dictionary dictionary
    (let ((string (ndeb-dictionary-get-info dictionary "search methods"))
          menu content image_menu image_content)
      (when string
        (when (string-match  " menu\\($\\| \\)" string)
          (setq menu    (list (ndeb-new-entry 'regular "menu" "[Menu]"))
                content (ndeb-process-require "menu"))
          (lookup-put-property menu 'ndeb-content content))
        (when (string-match  " image_menu\\($\\| \\)" string)
          (setq image_menu (list (ndeb-new-entry 'regular "image_menu" "[Graphic menu]"))
                image_content (ndeb-process-require "image_menu")
                menu (append menu image_menu))
          (lookup-put-property image_menu 'ndeb-content image_content))
        )
      (message "debug menu=%s" menu)
    menu)))

(defun ndeb-dictionary-copyright (dictionary)
  (ndeb-with-dictionary dictionary
    (list (ndeb-new-entry 'regular "copyright" "[Copyright]"))))

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
	  (ndeb-require-set "search-method"
			    (lookup-assq-ref 'ndeb-method-table method))
	  (lookup-put-property ndeb-current-agent 'ndeb-method method))
	(setq cmd (format "search \"%s\"" (ndeb-escape-query string)))))
      (ndeb-process-require cmd
        (lambda (process)
	  (let (code heading dupchk entries)
	    (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)\t\\(.*\\)" nil t)
	      (setq code (match-string 1) heading (match-string 2))
	      ;; 同じエントリがあるかチェックする。
	      ;; これがけっこうあるんだ・・
	      (unless (member (cons code heading) dupchk)
		(setq entries (cons (ndeb-new-entry 'regular code heading) entries))
		(setq dupchk (cons (cons code heading) dupchk))))
	    (nreverse entries)))))))

(defun ndeb-entry-content (entry)
  (or (lookup-get-property entry 'ndeb-content)
      (ndeb-with-dictionary (lookup-entry-dictionary entry)
        (let ((stop (ndeb-dictionary-stopcode dictionary))
              (last (lookup-get-property ndeb-current-agent 'ndeb-stop)))
          (unless (eq stop last)
            ;; 必要のあるときだけ stop-code を設定する。ndeb-dict に同じ。
            (ndeb-require-set "stop-code" stop)
            (lookup-put-property ndeb-current-agent 'ndeb-stop stop)))
        (let ((code (lookup-entry-code entry)) return)
          (ndeb-require-set "decorate-mode" "on")
          (setq return
                (if (member code '("menu" "image_menu" "copyright"))
                    (ndeb-process-require code)
                  (ndeb-process-require (concat "content "
                                                (lookup-entry-code entry)))))
          (ndeb-require-set "decorate-mode" "off")
          return))))
	  

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
      (setq height (or height ndeb-gaiji-default-size))
      (lookup-dictionary-set-option dictionary
                                    :gaiji-size height)
      (ndeb-require-set "font" (number-to-string height)))
    (setq xbm (ndeb-process-require (concat "font " code)))
    (catch :done
      (when (string-match "default_width" xbm)
        (throw :done (list (vector 'xbm xbm))))
      ;; check later
      (when (equal (ndeb-process-require "show font") "16")
        (throw :done nil))
      (ndeb-require-set "font" "16")
      (setq xbm (ndeb-process-require (concat "font " code)))
      (when  (string-match "default_width" xbm)
        (list (vector 'xbm xbm))))))


;;;
;:: Internal functions
;;;

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

(defun ndeb-require-set (var value)
  (if value
      (ndeb-process-send (format "set %s \"%s\"" var value))
    (ndeb-process-send (format "unset %s" var))))

(defun ndeb-escape-query (string)
  (let ((start 0))
    (while (string-match "\\\\" string start)
      (setq string (replace-match "\\\\" t t string)
	    start (1+ (match-end 0)))))
  string)

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

(defun ndeb-arrange-ignore (entry)
  (let ((regexp (lookup-dictionary-option dictionary :ignore-regexp t)))
    (while (re-search-forward regexp nil t)
      (replace-match ""))))

(defalias 'ndeb-arrange-fill-lines 'lookup-arrange-fill-lines)

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
           (ndebs-entry-content
            (lookup-new-entry 'regular dictionary code nil))))))))

;;;
;:: eblook process
;;;

(defun ndeb-process-open (directory appendix)
  (let* ((args (append ndeb-program-arguments
		       (cons directory (if appendix (list appendix)))))
	 (buffer (lookup-open-process-buffer (concat " *ndeb+" directory "*")))
	 (process (apply 'start-process "ndeb"
			 (or buffer (lookup-temp-buffer))
			 ndeb-program-name args)))
    (process-kill-without-query process)
    (accept-process-output process)
    (with-current-buffer (process-buffer process)
      (save-excursion
	(goto-char (point-min))
	(if (search-forward "Warning: invalid book directory" nil t)
	    (error "Invalid dictionary directory: %s" directory))
	(goto-char (point-min))
	(if (search-forward "Warning: invalid appendix directory" nil t)
	    (error "Invalid appendix directory: %s" appendix)))
      (process-send-string process "set prompt\n")
      (insert "set prompt\n")
      (unless buffer
	(set-process-buffer process nil)
	(kill-buffer (current-buffer))))
    process))

(put 'ndeb-process-require 'lisp-indent-function 1)
(defun ndeb-process-require (command &optional filter)
  (let ((lookup-process-output-separator-lines 0))
    (lookup-process-require ndeb-current-process
			    (concat command "\nset prompt \""
				    ndeb-prompt-string "\"\nset prompt\n")
			    (concat "^" ndeb-prompt-string) filter)))

(defun ndeb-process-send (string)
  (lookup-process-send ndeb-current-process (concat string "\n")))

(provide 'ndeb)

;;; ndeb.el ends here
