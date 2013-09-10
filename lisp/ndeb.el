;;; ndeb.el --- eblook interface -*- lexical-binding: t -*-
;; Copyright (C) 2006 Kazuhiro Ito <kzhr@d1.dion.ne.jp>

;; Author: Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;;         KAWABATA, Taichi <kawabata.taichi@gmail.com>

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

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'lookup)
(defconst ndeb-version "0.1")

(autoload 'ndeb-arrange-xbm "ndeb-binary")
(autoload 'ndeb-arrange-bmp "ndeb-binary")
(autoload 'ndeb-arrange-jpeg "ndeb-binary")
(autoload 'ndeb-arrange-image-page "ndeb-binary")
(autoload 'ndeb-arrange-wave "ndeb-binary")
(autoload 'ndeb-arrange-mpeg "ndeb-binary")
(autoload 'ndeb-arrange-snd-autoplay "ndeb-binary")
(autoload 'ndeb-binary-clear "ndeb-binary")

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

;; '-i' may be attached to MacPorts version of eblook.
(defcustom ndeb-program-arguments
  (if (equal system-type 'darwin) '("-q" "-e" "euc-jp" "-i")
    '("-q" "-e" "euc-jp"))
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

(defcustom ndeb-max-text 81920
  "*Maximum text size for ndeb agent.
If you encounter \"<more point=5>\" or similar message at the end
of buffer, please increase this size, or set to 0 for unlimited."
  :type 'integer
  :group 'ndeb)

(defcustom ndeb-gaiji-size 16
  "デフォルトで使用する外字のサイズ。指定したサイズの外字が存在し
ない場合は指定値を越えない最大サイズを、それも存在しない場合は16ドッ
トの外字を使用する。"
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

(defcustom ndeb-gaiji-map-directory (concat lookup-init-directory
                                            "/epwing-gaiji")
  "Default directory to search for GAIJI file if not found in
dictionaries' directory."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-alternate-file (concat ndeb-gaiji-map-directory
                                    "/alternate.ini")
  "File that describes query character filter table."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-encode-directory-name
  ;; On Japanese Windows, eblook handles non-ASCII file name
  ;; correctly.
  (null (and (memq system-type '(windows-nt ms-dos))
	     (memq (coding-system-base
		    (or file-name-coding-system
			default-file-name-coding-system))
		   '(japanese-cp932 japanese-shift-jis))))
  "When non-nil, encode directory name before passing it to eblook process."
  :type 'boolean
  :group 'ndeb)


;;;
;;; Internal Variables
;;;

(defvar ndeb-process nil
  "Process object for ndeb agents.")

(defvar ndeb-vars nil
  "process variables cache.")

(defvar ndeb-current-agent nil)

(defvar ndeb-current-dictionary nil)

(defvar ndeb-support-escape-text nil
  "Positive numeric value means that eblook supports text escape.
Zero or negative value mean that feature is not supported.
Nil means it has not been checked yet.")

(defvar ndeb-alternate-table nil)
(defvar ndeb-alternate-regexp nil)


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
  '(("italic" . lookup-italic-face)
    ("bold" . lookup-bold-face)
    ("em" . lookup-emphasis-face)))

;;;
;:: inline functions
;;;

(defsubst ndeb-ebnet-uri-p (string)
  "Return non-nil if STRING is ebnet uri."
  (save-match-data
    (let ((case-fold-search t))
      (string-match "^ebnet://" string))))

;;;
;;; macros & basic functions
;;;

;;; Get/Set functions

(defun ndeb-vars-get (var)
  (lookup-assoc-get ndeb-vars var))

(defun ndeb-vars-set (var value)
  (setq ndeb-vars
	(lookup-assoc-put ndeb-vars var value)))

(defmacro ndeb-with-agent (agent &rest body)
  (declare (indent 1))
  "Switch eblook's context to AGENT and execute BODY."
  `(let (book appendix command filter localp)
     (ndeb-process-open)
     (unless (eq ,agent ndeb-current-agent)
       (setq book (lookup-agent-location ,agent)
	     appendix (lookup-agent-option ,agent :appendix))
       (when (setq localp (null (ndeb-ebnet-uri-p book)))
	 ;; We assume book and appendix locations are both local or
	 ;; remote.
	 (setq book (expand-file-name book)
	       appendix (and appendix (expand-file-name appendix))))
       (setq command (concat
		      "book "
		      (mapconcat
		       (lambda (elt)
			 (and elt (replace-regexp-in-string
				   "\\([\\ \t]\\)" "\\\\\\1" elt t nil)))
		       (list book appendix)
		       " "))
	     filter (lambda (_process)
		      (if (search-forward "invalid book" nil t)
			  (error "Invalid dictionary directory: %s" book))
		      (if (search-forward "invalid appendix" nil t)
			  (error "Invalid appendix directory: %s" book))))
       (if (and ndeb-encode-directory-name localp)
	   (lookup-with-coding-system
	       (let ((eol (coding-system-eol-type ndeb-process-coding-system)))
		 (coding-system-change-eol-conversion
		  'raw-text (and (integerp eol) eol)))
	     (ndeb-process-require
	      (encode-coding-string
	       command
	       (or file-name-coding-system default-file-name-coding-system))
	      filter))
	 (ndeb-process-require command filter))
       (setq ndeb-current-agent ,agent)
       (lookup-put-property ndeb-current-agent :ndeb-dict nil))
     ,@body))

(defmacro ndeb-with-dictionary (dictionary &rest body)
  (declare (indent 1))
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
  (declare (indent 1))
  (let ((lookup-process-output-separator-lines 0))
    (lookup-process-require 
     ndeb-process
     (concat command "\n")
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
    ;; (ndeb-process-require-set "prompt" ndeb-prompt-string)
    (ndeb-process-require-set "decorate-mode" "on")

    ;; Check if eblook support escape text feature.
    (unless ndeb-support-escape-text
      (if  (string-match "^escape-text\t" (ndeb-process-require "show"))
	  (setq ndeb-support-escape-text 1)
	(setq ndeb-support-escape-text 0)))
    (when (> ndeb-support-escape-text 0)
      (ndeb-process-require-set "escape-text" "on"))))

(defun ndeb-new-entry (type code &optional heading)
  (lookup-new-entry type ndeb-current-dictionary code heading))


;;;
;;; Interaces
;;;

;; arrangements
(put 'ndeb :arrange-table
     '((replace   ndeb-arrange-auto-jump-reference
                  lookup-arrange-replaces
                  ndeb-arrange-xbm
                  ndeb-arrange-bmp
                  ndeb-arrange-jpeg
                  ndeb-arrange-image-page
                  ndeb-arrange-wave
                  ndeb-arrange-mpeg
                  ndeb-arrange-unicode
                  ndeb-arrange-scripts
                  ndeb-arrange-faces
                  ndeb-arrange-no-newline
                  ndeb-arrange-decode-entity)
       (gaiji     lookup-arrange-gaijis)
       (reference ndeb-arrange-paged-reference
                  ndeb-arrange-squeezed-references
                  lookup-arrange-references)
       (structure ndeb-arrange-prev-next ; to obtain prev/next entry.
                  ndeb-arrange-indent
                  lookup-arrange-structure)
       (fill      lookup-arrange-fill-lines
                  ndeb-arrange-snd-autoplay)))

(put 'ndeb :reference-pattern
     '("<reference>\\(→?\\(\\(.\\|\n\\)*?\\)\\)</reference=\\([^>]+\\)>" 1 2 4))
(put 'ndeb :charsets
     '(ascii japanese-jisx0213.2004-1 japanese-jisx0213-2 japanese-jisx0212))
(put 'ndeb :stop-code     nil)

(put 'ndeb :query-filter 'ndeb-query-filter-alternate)

(put 'ndeb :list 'ndeb-list)
(defun ndeb-list (agent)
  (when ndeb-program-name
    (ndeb-with-agent agent
      (ndeb-process-require
       "list"
       (lambda (_process)
         (let (dicts)
           (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)" nil t)
             (push (lookup-new-dictionary ndeb-current-agent (match-string 1))
                   dicts))
           (nreverse dicts)))))))

(put 'ndeb :kill 'ndeb-kill)
(defun ndeb-kill (agent)
  (ndeb-agent-kill-process agent)
  (ndeb-binary-clear agent))

(put 'ndeb :title 'ndeb-dictionary-title)
(defun ndeb-dictionary-title (dictionary)
  (ndeb-dictionary-get-info dictionary "title"))

(put 'ndeb :methods 'ndeb-dictionary-methods)
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

(put 'ndeb :menu 'ndeb-dictionary-menu)
(defun ndeb-dictionary-menu (dictionary)
  (ndeb-initialize-dictionary dictionary)
  (ndeb-with-dictionary dictionary
  (let ((string (ndeb-dictionary-get-info dictionary "search methods"))
        menu content image-menu image-content copyright copyright-content)
    (when string
      (when (string-match  " menu\\($\\| \\)" string)
        (setq menu    (list (ndeb-new-entry 'regular "menu" "[Menu]"))
              content (ndeb-process-require "menu"))
        (lookup-put-property menu 'ndeb-content content))
      (when (string-match  "image_menu" string)
        (setq image-menu (list (ndeb-new-entry 'regular
                                               "image_menu" "[Graphic menu]"))
              image-content (ndeb-process-require "image_menu"))
        (lookup-put-property image-menu 'ndeb-content image-content))
      (when (string-match "copyright" string)
        (setq copyright (list (ndeb-new-entry 'regular 
                                              "copyright" "[Copyright]"))
              copyright-content (ndeb-process-require "copyright"))
        (lookup-put-property copyright 'ndeb-content copyright-content)))
  (nconc menu image-menu copyright))))

(put 'ndeb :search 'ndeb-dictionary-search)
(defun ndeb-dictionary-search (dictionary query)
  "Search EB DICTIONARY for QUERY."
  (ndeb-initialize-dictionary dictionary)
  (ndeb-with-dictionary dictionary
    (let* ((method (lookup-query-method query))
           (string (ndeb-escape-kanji (lookup-query-string query)))
           (last-method (lookup-get-property ndeb-current-agent 'ndeb-method))
           (ndeb-method (or (lookup-assq-get ndeb-method-table method)
                            (and (equal method 'keyword) "cross")
                            (and (equal method 'substring) "wild")))
           (command
            (case method
              ('keyword
               (concat
                "set search-method keyword\n"
                "search \"=" (replace-regexp-in-string "[ \t　]+" "=" string) "\"\n"
                "set search-method cross\n"
                "search \"&" (replace-regexp-in-string "[ \t　]+" "&" string) "\""))
              ('substring
               (concat "search \"*" string "*\""))
              (t 
               (concat "search \"" (ndeb-escape-query string) "\"")))))
      (unless (eq method last-method)
        ;; 必要のあるときだけ search-method を設定する。ndeb-dict に同じ。
        (ndeb-process-require-set "search-method" ndeb-method)
        (lookup-put-property ndeb-current-agent 'ndeb-method method))
      (ndeb-process-require command
        (lambda (_process)
	  (let (results)
	    (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)\t\\(.*\\)" nil t)
              (push (cons (match-string 1) (match-string 2)) results))
            ;; 重複の削除
            (setq results (delete-dups results))
            (mapcar (lambda (result)
                      (ndeb-new-entry 'regular (car result) (cdr result)))
                    (nreverse results))))))))

(put 'ndeb :gaiji-regexp  "<gaiji=\\([^>]*\\)>")
(put 'ndeb :gaiji 'ndeb-dictionary-gaiji)
(defun ndeb-dictionary-gaiji (dictionary code)
  (ndeb-with-dictionary dictionary
    (let ((height (lookup-dictionary-option dictionary :gaiji-size t))
          (tmp    (ndeb-dictionary-get-info dictionary "font sizes"))
          xbm)
      (when (string-match "[0-9]+$" tmp) ;; max size
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
      (ndeb-process-require-set "font" (number-to-string height))
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
          (list (vector 'xbm xbm)))))))

(put 'ndeb :content 'ndeb-entry-content)
(defun ndeb-entry-content (entry)
  (ndeb-initialize-dictionary (lookup-entry-dictionary entry))
  (or (lookup-get-property entry 'ndeb-content)
      (ndeb-with-dictionary (lookup-entry-dictionary entry)
        (if ndeb-max-text 
            (ndeb-process-require-set "max-text" ndeb-max-text))
        ;; processing `stop-code' if necessary.
        (let* ((dictionary (lookup-entry-dictionary entry))
               (stop (lookup-dictionary-option dictionary :stop-code t))
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

(defun ndeb-initialize-dictionary (dictionary)
  (let ((initialized (lookup-dictionary-option dictionary :initialized)))
    (unless initialized
      (let* ((agent (lookup-dictionary-agent dictionary))
             (agent-location (lookup-agent-location agent))
	     (localp (null (ndeb-ebnet-uri-p agent-location)))
	     agent-name dict-name dict-location gaiji-file)
	(when localp
	  (setq agent-name (file-name-nondirectory agent-location)
		;; Emacs BUG :: directory-files can't do CI search
		;; even if case-fold-search is t.
		dict-name (upcase (lookup-dictionary-name dictionary))
		dict-location (concat agent-location "/" dict-name)))
	(setq gaiji-file
              (or (lookup-agent-option agent :gaiji-file)
		  (when localp
		    (or (and (file-directory-p dict-location)
			     (car (directory-files dict-location t "\\.map$")))
			(and (file-directory-p ndeb-gaiji-map-directory)
			     (car (or (directory-files
				       ndeb-gaiji-map-directory t
				       (concat dict-name ".map"))
				      (directory-files
				       ndeb-gaiji-map-directory t
				       (concat agent-name ".map")))))))))
        (if gaiji-file
            (setf (lookup-dictionary-option dictionary :gaiji-table)
                  (ndeb-parse-gaiji-file gaiji-file))))
      (lookup-dictionary-set-option dictionary :initialized t))))

(defun ndeb-parse-gaiji-file (gaiji-file)
  "Return lookup-gaiji-table from specified EBStudio-style GAIJI-FILE."
  (let ((gaiji-data))
    (with-temp-buffer
      (insert-file-contents-literally gaiji-file)
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([hz][A-F][0-9A-F]+\\)	\\([u0-9A-F,]+\\)" nil t)
        (let* ((gaiji (match-string 1))
               (ucs (match-string 2))
               (ucs (mapconcat (lambda (x)
                                 (char-to-string
                                  (string-to-number x 16)))
                               (split-string ucs "[u,]" t)
                               "")))
          (setq gaiji-data (cons (list (downcase gaiji) ucs)
                                 gaiji-data)))))
    (lookup-new-gaiji-table gaiji-data)))

(defun ndeb-setup-query-filter-alternate ()
  (or ndeb-alternate-regexp
      (when (file-exists-p ndeb-alternate-file)
        (with-temp-buffer
          (insert-file-contents ndeb-alternate-file)
          (setq ndeb-alternate-table (make-hash-table))
          (while (re-search-forward "^u\\([0-9A-F]+\\)\t\\([^\t\n]+\\)\\>" nil t)
            (unless (equal " " (match-string 1))
              (puthash (string-to-number (match-string 1) 16) (match-string 2)
                       ndeb-alternate-table)))
          (setq ndeb-alternate-regexp
                (regexp-opt
                 (loop for char being the hash-keys of ndeb-alternate-table
                   collect (char-to-string char))))))))

(defun ndeb-string-filter-alternate (string)
  (if (ndeb-setup-query-filter-alternate)
      (replace-regexp-in-string
       ndeb-alternate-regexp
       (lambda (str) (gethash (string-to-char str) ndeb-alternate-table))
       string)
    string))

(defun ndeb-query-filter-alternate (query)
  (lookup-new-query-filter query 'ndeb-string-filter-alternate))

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
		(lambda (_process)
                  (loop while (re-search-forward "^ \\([^:]+\\): \\(.*\\)" nil t)
                        collect (cons (match-string 1) (match-string 2)))))))
      (lookup-put-property dictionary 'ndeb-alist alist))
    (lookup-assoc-get alist key)))

(defun ndeb-escape-kanji (string)
  (replace-regexp-in-string 
   "\\cC"
   (lambda (ch) (if (encode-char (string-to-char ch) 'japanese-jisx0208)
                    ch (format "%X" (string-to-char ch))))
   string))

(defun ndeb-escape-query (string)
  (replace-regexp-in-string "\\\\" "\\\\" string t t))



;;;
;;; Arrange fnctions
;;;

(defun ndeb-arrange-squeezed-references (entry)
  (if (lookup-dictionary-option
       (lookup-entry-dictionary entry) :squeezed nil)
      (while (search-forward-regexp "→□\\(#0001\\|<gaiji:z0001>\\)?" nil t)
	(replace-match ""))))

(defun ndeb-arrange-no-newline (_entry)
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

(defun ndeb-arrange-paged-reference (_entry)
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
  (let ((dictionary (lookup-entry-dictionary entry))
        code)
    (when (re-search-forward "<auto-jump-reference></auto-jump-reference=\\([0-9]+:[0-9]+\\)>" nil t)
      (setq code (match-string 1))
      (delete-region (point-min) (point-max))
      (insert
       (if (eq (lookup-agent-class (lookup-dictionary-agent dictionary)) 'ndeb)
           (ndeb-entry-content (ndeb-new-entry 'regular code nil))
         (ndeb-entry-content
          (lookup-new-entry 'regular dictionary code nil))))))))

(defun ndeb-arrange-indent (entry)
  (let ((dictionary (lookup-entry-dictionary entry)))
  (while (re-search-forward "<ind=\\([0-9]\\)>" nil t)
    (let ((beg-beg (match-beginning 0))
	  ;; (beg-end (match-end 0))
	  (level (- (string-to-number (match-string 1))
		    (or 
		     (lookup-dictionary-option dictionary :minimum-indent t)
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
	(goto-char point))))))

(defun ndeb-arrange-unicode (_entry)
  (while (re-search-forward "<unicode>\\([0-9A-F０-９Ａ-Ｆ]+\\)</unicode>" nil t)
    (replace-match 
     (save-match-data
       (char-to-string 
        (string-to-number (japanese-hankaku (match-string 1)) 16))))))

(defun ndeb-arrange-scripts (entry)
  (let ((dictionary (lookup-entry-dictionary entry))
        beg-beg tag)
    (while (re-search-forward "<\\(su[bp]\\)>" nil t)
      (setq beg-beg (match-beginning 0)
            tag     (match-string 1))
      (delete-region beg-beg (match-end 0))
      (if (and (re-search-forward (concat "<\\(/?\\)" tag ">") nil t)
	       (equal (match-string 1) "/"))
	  (let ((end-beg (match-beginning 0))
		(height (or (lookup-dictionary-option
			     dictionary :script-height t)
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

(defun ndeb-arrange-faces (_entry)
  (while (re-search-forward "<\\(/?\\)em>" nil t)
    (if (equal (match-string 1) "/")
	(replace-match "</font>" t t)
      (replace-match "<font=em>" t t)))
  (goto-char (point-min))
  (while (re-search-forward "<font=\\([a-z]+\\)>" nil t)
    (let ((beg-beg (match-beginning 0))
	  (beg-end (match-end 0))
	  (class (match-string 1))
          (nest-count 1))
      (while (and (/= nest-count 0)
                  (re-search-forward
                   "<\\(/?\\)font\\(=[a-z]+\\)?>" nil t))
        (if (equal (match-string 1) "/")
            (setq nest-count (1- nest-count))
          (setq nest-count (1+ nest-count)))
        (when (= nest-count 0)
	  (let ((end-beg (match-beginning 0))
		(end-end (match-end 0)))
	    (add-text-properties beg-end end-beg
				 `(face ,(or (lookup-assoc-get
					      ndeb-faces-table class)
					     'lookup-italic-face))) ;; default
	    (delete-region end-beg end-end)
	    (delete-region beg-beg beg-end)
            (goto-char beg-beg))))
      (when (/= nest-count 0)
	(goto-char beg-beg)
	(delete-region beg-beg beg-end)))))

(defun ndeb-arrange-decode-entity (_entry)
  (when (> ndeb-support-escape-text 0)
    (while (re-search-forward "&\\(amp\\|lt\\|gt\\);" nil t)
      (let* ((pos (match-beginning 0))
	     (properties (text-properties-at pos)))
	(replace-match (lookup-assoc-get
			ndeb-entities-table (match-string 1)) t t)
	(set-text-properties pos (1+ pos) properties)
	(goto-char (1+ pos))))))

(provide 'ndeb)

;;; ndeb.el ends here
