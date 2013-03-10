;;; ndest.el --- search agent for Hyper Estraier -*- lexical-binding: t -*-
;; Copyright (C) 2007 Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;; Copyright (C) 2009 Lookup Development Team

;; Author: Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;; Modified by: Taichi KAWABATA <kawabata.taichi@gmail.com>

;; ndest.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License.

;; ndest.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'lookup)
(require 'url-http)
(declare-function navi2ch-bookmark-load-info "ext:navi2ch.el")
(declare-function navi2ch-find-file "ext:navi2ch.el")
(declare-function mime-view-buffer "ext:semi.el")

(defconst ndest-version "0.1")

;;;
;;; Customizable variables
;;;

(defgroup ndest nil
  "Lookup Hyper Estraier interface."
  :group 'lookup-agents)

(defcustom ndest-estcall-program-name "estcall"
  "Program name of estcall."
  :type 'string
  :group 'ndest)

(defcustom ndest-estcmd-program-name "estcmd"
  "Program name of estcmd."
  :type 'string
  :group 'ndest)

(defcustom ndest-process-coding-system-for-write 'utf-8
  "Coding system for writing to estcmd process."
  :type 'symbol
  :group 'ndest)

(defcustom ndest-iconv-coding-system-for-write nil
  "An iconv name of coding system for writing to estcmd process. Nil means set automatically."
  :type '(choice (const nil) string)
  :group 'ndest)

(defcustom ndest-default-proxy nil
  "Cons of default proxy server name and port number."
  :type '(choice (const nil :tag "Use default")
		 (const t :tag "Force direct access")
		 (cons :tag "Set proxy"
		       (string :tag "server name")
		       (number :tag "port number")))
  :group 'ndest)

(defcustom ndest-default-auth nil
  "Cons of username and password."
  :type '(choice (const nil :tag "none")
		 (cons :tag "Set user/pass"
		       (string :tag "username")
		       (string :tag "password")))
  :group 'ndest)

(defcustom ndest-max-text 2048
  "*検索時に表示するエントリ本文の最大長。
0 を指定すると、estcmdのデフォルトに従う。
nilの場合は `lookup-max-text' の値を参照する。
estcall使用時は無効。"
  :type '(choice (const nil) integer)
  :group 'ndest)

(defcustom ndest-follow-link-functions
  '(("^message/rfc822$" nil ndest-follow-link-with-mime-view)
    (nil "/\\.navi2ch/.+\\.dat$" ndest-follow-link-with-navi2ch)
    ("^text/html$" nil ndest-follow-link-with-w3m))
  "A list of arrange functions for content. Each element is a list consist of regexp for data-type, uri and symbol of function."
  :type '(repeat (list (choice :tag "type" regexp (const nil))
		       (choice :tag "URI" regexp (const nil))
		       (choice (function :tag "function")
			       (string :tag "program")
			       (cons :format "%v"
				     :tag "program with args"
				     (string :tag "link-program")
				     (repeat :tag "parameters")
				     ))))
  :group 'ndest)

(defcustom ndest-arrange-functions
  '(("^message/rfc822$" nil ndest-arrange-rfc822)
    ("^text/plain$" "/\\.navi2ch/.+\\.dat$" ndest-arrange-navi2ch-dat))
  "A list of arrange functions for content. Each element is a list consist of regexp for data-type, uri and symbol of function."
  :type '(repeat (list (choice :tag "type" regexp (const nil))
		       (choice :tag "URI" regexp (const nil))
		       (function :tag "arrange-function")))
  :group 'ndest)

(defcustom ndest-program-arguments '("-sf")
  "A list of arguments for estcmd."
  :type '(repeat (string :tag "option"))
  :group 'ndest)

(defcustom ndest-follow-link-from-entry nil
"A string or a vector of symbols and characters meaning a sequence of keystrokes and events for `lookup-entry-follow-ndest-link'. If nil no keystrokes are assigned."
:type '(choice (const nil) (string :tag "Key"))
:group 'ndest)


;;;
;;; types
;;;

(put 'ndest ':methods '(text keyword))

(put 'ndest ':arranges
     '((structure ndest-arrange-content)))

(put 'ndest ':adjusts
     '(lookup-adjust-goto-min))

(put 'ndest ':default-method
     'text)


;;;
;;; Internal variables
;;;

(defvar ndest-mime-raw-buffer nil)

(defvar ndest-mime-view-buffer nil)

(defvar ndest-link-map nil
  "Keymap for ndest links.")

(defconst ndest-coding-system-table
  '(
    ("^euc-jisx0213"      . "EUC-JISX0213")
    ("^japanese-iso-8bit" . "EUC-JP")
    ;; ("^euc-japan-1990"          . "EUC-JP")
    ("^euc-j"             . "EUC-JP")
    ("shift[-_]jisx0213"  . "Shift_JISX0213")
    ("shift[-_]jis"       . "Shift_JIS")
    ("^sjis"              . "Shift_JIS")
    ("^cp932"             . "Shift_JIS")
    ("^utf-8"             . "UTF-8")
    ("^iso-2022-jp"       . "ISO-2022-JP")
    ("^junet"             . "ISO-2022-JP")
    ))

;;;
;;; Interface functions
;;;

(put 'ndest :list 'ndest-list)
(defun ndest-list (agent)
  (let* ((location (lookup-agent-location agent)))
    (if (or (string-match "^http://" location)
            (file-directory-p location))
        (list (lookup-new-dictionary agent "")))))

(put 'ndest :kill 'ndest-kill)
(defun ndest-kill (_agent)
  (when (buffer-live-p ndest-mime-raw-buffer)
    (kill-buffer ndest-mime-raw-buffer))
  (when (buffer-live-p ndest-mime-view-buffer)
    (kill-buffer ndest-mime-view-buffer)))

(put 'ndest :title 'ndest-title)
(defun ndest-title (_dictionary)
  "HyperEstraier Search")

(put 'ndest :search 'ndest-dictionary-search)
(defun ndest-dictionary-search (dictionary query)
  (with-temp-buffer
    (if (string-match "^http://" (lookup-agent-location
				  (lookup-dictionary-agent dictionary)))
        (ndest-dictionary-search-node dictionary query)
      (ndest-dictionary-search-with-estcmd dictionary query))
    (goto-char (point-min))
    (when (re-search-forward "^--------\\[[0-9A-F]+\\]--------" nil t)
      (let ((sep (concat "^" (regexp-quote (match-string 0)) "\\(:END\\)?$")))
	(when (re-search-forward sep nil t)
	  (let (start end end-end entries title type)
	    (setq start (match-end 0))
	    (while (re-search-forward sep nil t)
	      (setq end (match-beginning 0))
	      (setq end-end (match-end 0))
	      (narrow-to-region start end)
	      (let ((uri (ndest-get-header-string "@uri")))
		(setq title (or (ndest-get-header-string "@title")
				(and (ndest-uri-is-file uri)
				     (ndest-get-header-string "_lfile")) ;; _lpath
				uri)))
	      (setq type (or (ndest-get-header-string "@type") "unknown"))
	      (setq entries 
		    (cons (lookup-new-entry 'regular dictionary (buffer-string)
					     (concat title " (" type ")"))
			  entries))
	      (widen)
	      (goto-char end-end)
	      (setq start end-end))
	    (nreverse entries)))))))
   
(defun ndest-dictionary-search-with-estcmd (dictionary query)
  (let ((coding-system-for-write ndest-process-coding-system-for-write)
	(coding-system-for-read 'utf-8)
	(args  '("search" "-vs"))
	ic)
    (if ndest-iconv-coding-system-for-write
	(setq ic ndest-iconv-coding-system-for-write)
      (setq ic (symbol-name ndest-process-coding-system-for-write))
      (let ((params ndest-coding-system-table)
	    param)
	(while params
	  (setq param (car params))
	  (when (string-match (car param) ic)
	    (setq ic (cdr param))
	    (setq params nil))
	  (setq params (cdr params)))))
    (setq args 
	  (append
	   args (or (lookup-dictionary-option dictionary ':args t)
		    ndest-program-arguments)
	   (when lookup-max-hits
	     (if (eq lookup-max-hits 0)
		 (list "-max" "-1")
	       (list "-max" (number-to-string lookup-max-hits))))
	   (list "-ic" ic)
	   (let ((max-text
		  (or (lookup-dictionary-option dictionary ':max-text t)
		      ndest-max-text
		      lookup-max-text)))
	     (when max-text
	       (unless (eq max-text 0)
		 (let ((half (number-to-string (/ max-text 2))))
		   (list "-sn" (number-to-string max-text)
			 half half)))))
	   (list (expand-file-name (lookup-agent-location
                  (lookup-dictionary-agent dictionary)))
		 (lookup-query-string query))))
    (message "debug: args=%s" args)
    (apply 'call-process ndest-estcmd-program-name nil t nil args)))
   
(defun ndest-dictionary-search-with-estcall (dictionary query)
  (let ((coding-system-for-write 'utf-8)
	(coding-system-for-read 'utf-8)
	(args  '("search")))
    (setq args 
	  (append
	   args
	   (or (lookup-dictionary-option dictionary ':args t)
	       ndest-program-arguments)
	   (when lookup-max-hits
	     (if (eq lookup-max-hits 0)
		 (list "-max" "-1")
	       (list "-max" (number-to-string lookup-max-hits))))
	   (let ((proxy (or (lookup-dictionary-option dictionary ':proxy t)
			    ndest-default-proxy)))
	     (when (consp proxy)
	       (list "-proxy" (car proxy) (number-to-string (cdr proxy)))))
	   (let ((auth (or (lookup-dictionary-option dictionary ':auth t)
			   ndest-default-auth)))
	     (when (consp auth)
	       (list "-auth" (car auth) (cdr auth))))
	   (list (lookup-agent-location
                  (lookup-dictionary-agent dictionary))
		 (lookup-query-string query))))
    (apply 'call-process ndest-estcall-program-name nil t nil args)))

(eval-when-compile
  (condition-case nil
      (require 'url-parse)
    (error nil)))

(defun ndest-dictionary-search-node (dictionary query)
  (let ((arg "/search?")
	uri data)
    (setq arg
	  (concat
	   arg
	   (when lookup-max-hits
	     (format "max=%d&"
		     (if (eq lookup-max-hits 0) -1
		       lookup-max-hits)))
	   (let ((max-text
		  (or (lookup-dictionary-option dictionary ':max-text t)
		      ndest-max-text
		      lookup-max-text)))
	     (when max-text
	       (unless (eq max-text 0)
		 (let ((half (/ max-text 2)))
		   (format "wwidth=%d&hwidth=%d&awidth=%d&"
			   max-text half half)))))
	   "phrase=" (ndest-url-encode-string
		      (ndest-normalize-query-string
		       (lookup-query-string query)))))
    (setq uri (url-generic-parse-url
	       (concat (lookup-agent-location
                        (lookup-dictionary-agent dictionary)) arg)))
    ;; URLに含まれたユーザ名/パスワードは使わないようなので
    ;; 直接 url-http-real-basic-auth-storage に入れている。
    (let ((auth (or (lookup-dictionary-option dictionary ':auth t)
		    ndest-default-auth))
	  ;;(url-http-real-basic-auth-storage url-http-real-basic-auth-storage)
	  (proxy (or (lookup-dictionary-option dictionary ':proxy t)
		     ndest-default-auth))
	  (url-proxy-services url-proxy-services))
      (cond 
       ((consp auth)
	(let ((key (format "%s:%s" (url-host uri) (url-port uri)))
	      (dir (let ((str (url-filename uri)))
		     (string-match "\\(^\\(/\\)[^/]*$\\|\\([^/]+\\)/[^/]*$\\)"
				   str)
		     (or (match-string 2 str)
			 (match-string 3 str)))))
	  (setq url-http-real-basic-auth-storage
		(cons (list key
			    (cons dir 
				  (base64-encode-string
				   (format "%s:%s" (car auth) (cdr auth)))))
		      (lookup-assoc-del
		       url-http-real-basic-auth-storage key)))))
       (auth (setq url-http-real-basic-auth-storage
		   (lookup-assoc-del url-http-real-basic-auth-storage key))))
      (cond
       ((consp proxy)
        (lookup-assoc-set 'url-proxy-services "http"
                          (format "%s:%d" (car proxy) (cdr proxy)))
       (proxy
	(setq url-proxy-services
	      (lookup-assoc-del url-proxy-services "http"))))
      (setq data (url-retrieve-synchronously uri)))
    (insert (decode-coding-string
             (with-current-buffer data (buffer-string)) 'utf-8)))))

(put 'ndest :content 'ndest-entry-content)
(defun ndest-entry-content (entry)
  (lookup-entry-code entry))

;;;
;:: Internal functions
;;;

(defun ndest-initialize ()
  "Initialize ndest module."
  (unless ndest-link-map
    (setq ndest-link-map (copy-keymap lookup-content-mode-map))
    (define-key ndest-link-map "\C-m" 'ndest-follow-link)
    (define-key ndest-link-map "u" 'ndest-show-uri)
    (if (featurep 'xemacs)
	(define-key ndest-link-map 'button2 'ndest-mouse-follow)
      (define-key ndest-link-map [mouse-2] 'ndest-mouse-follow))))

(defun ndest-get-header-string (header)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (when (re-search-forward
	     (concat "^" (regexp-quote header) "=\\(.+\\)$") nil t)
	(match-string 1)))))

(defun ndest-set-link (start end face type uri file)
  (let ((binary (list (cons 'type type)
		      (cons 'uri uri)
		      (cons 'file file))))
    (add-text-properties start end
			 (list 
			  (if (< emacs-major-version 21) 'local-map
			    'keymap)
			  ndest-link-map
			  'face (or face 'lookup-reference-face)
			  'mouse-face 'highlight
			  'help-echo (format
				      "[%s] mouse-2: play"
				      type)
			  'lookup-tab-stop t
			  'ndest-link binary))))

(defun ndest-get-link (&optional pos)
  (get-text-property (or pos (point)) 'ndest-link))

;; From w3m-url-encode-string in w3m.el
(defun ndest-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "+")
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string into a list of chars.
	  (append (encode-coding-string (or str "")
					(or coding
					    'utf-8))
		  nil))))

(defun ndest-normalize-query-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (when (re-search-forward "^[ &|!]+" nil t)
      (replace-match "" t t))
    (when (re-search-forward "[ &|!]+$" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (catch ':done
      (let (word start end)
	(while t
	  (unless (re-search-forward "\\(\"[^\"]*\"\\|[^ |&!\"]+\\)" nil t)
	    (message "ndest:Unsupported pattern.")
	    (throw ':done t))
	  (setq word (match-string 0)
		start (match-beginning 0)
		end (match-end 0))
	  (delete-region start end)
	  (goto-char start)
	  (cond
	   ;; フレーズ検索
	   ((string-match "^\".*\"$" word)
	    (insert (substring word 1 -1)))
	   ;; ワイルドカード検索
	   ((string-match "^\\*.*\\*$" word)
	    (insert (concat "[RX] " (substring word 1 -1))))
	   ;; 前方一致検索
	   ((string-match "^\\*" word)
	    (insert (concat "[BW] " (substring word 1))))
	   ;; 後方一致検索
	   ((string-match "\\*$" word)
	    (insert (concat "[EW] " (substring word 0 -1))))
	   ;; 通常検索
	   (t (insert word)))
	  (unless (re-search-forward "[ |&!]+" nil t)
	    (throw ':done t))
	  (setq word (match-string 0)
		start (match-beginning 0)
		end (match-end 0))
	  (delete-region start end)
	  (goto-char start)
	  (cond
	   ;; OR検索
	   ((string-match "|" word)
	    (insert " OR "))
	   ;; ANDNOT検索
	   ((string-match "!" word)
	    (insert " ANDNOT "))
	   ;; AND検索
	   (t (insert " AND "))))))
    (buffer-string)))

;;;
;;; Link functions
;;;

(defun lookup-entry-follow-ndest-link ()
  (interactive)
  (unless (lookup-summary-content-visible-p)
    (lookup-summary-display-content))
  (let ((window (get-buffer-window lookup-content-buffer)))
    (and window (select-window window)))
  (switch-to-buffer (lookup-content-buffer))
  (save-excursion
    (goto-char (point-min))
    (let (point)
      (if (get-text-property (point) 'ndest-link)
	  (setq point (point))
	(setq point (next-single-property-change (point) 'ndest-link)))
      (if point
	  (progn
	    (goto-char point)
	    (ndest-follow-link))
	(message "No link")))))

(defun ndest-follow-link ()
  (interactive)
  (let* ((links (ndest-get-link (point)))
	 (type (lookup-assq-get links 'type))
	 (uri (lookup-assq-get links 'uri))
	 (file (or (lookup-assq-get links 'file)
		   (ndest-uri-to-filepath uri)))
	 params param fn)
    (setq params
	  (or (lookup-dictionary-option
	       (lookup-entry-dictionary lookup-content-entry)
	       ':functions t)
	      ndest-follow-link-functions))
    (catch ':done
      (while params
	(setq param (car params))
	(setq fn (nth 2 param))
	(when (and (or (null (car param))
		       (string-match (car param) type))
		   (or (null (nth 1 param))
		       (string-match (nth 1 param) uri)))
	  (if (symbolp fn)
	      (funcall fn uri file)
	    (when (stringp fn)
	      (setq fn (list fn)))
	    (apply 'start-process " *ndest-links*" nil (car fn) (or file uri) (cdr fn)))
	  (throw  ':done t))
	(setq params (cdr params)))
      (ndest-follow-link-default uri file))))

(defun ndest-show-uri (&optional arg)
  "カーソル位置のリンク先のURIを表示し、kill-ringに保存する。prefix argumentがあり、URIがfile://で始まる場合はパス名が保存される。"
  (interactive "P")
  (let* ((links (ndest-get-link (point)))
	 (uri (lookup-assq-get links 'uri)))
    (when arg
      (setq uri (or (and (ndest-uri-is-file uri)
			 (or (lookup-assq-get links 'file)
			     (ndest-uri-to-filepath uri)))
		    uri)))
    (message "%s" uri)
    (kill-new uri)))

(defun ndest-uri-is-file (uri)
  "uriがfile://で始まる場合はnon-nilを、それ以外の場合はnilを返す。"
  (string-match "^file://" uri))

(defun ndest-uri-to-filepath (uri)
  "uriがfile://で始まる場合はパス名に変換する。そうでない場合はnilを返す。"
  (when (ndest-uri-is-file uri)
    (with-temp-buffer
      (insert uri)
      (goto-char (point-min))
      (re-search-forward "^file://" nil t)
      (replace-match "" t t)
      (when (re-search-forward "^/\\([A-Za-z]\\)|/" nil t)
	(replace-match "\\1:/" nil nil))
      (while (re-search-forward "%\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)" nil t)
	(let ((ch (+ (* 16 (string-to-number (match-string 1) 16))
		     (string-to-number (match-string 2) 16))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (goto-char (match-beginning 0))
	  (insert-char ch 1)))
      (decode-coding-string (buffer-string) 'utf-8))))

(defun ndest-follow-link-with-mime-view (_uri file)
  "rfc822形式のファイルをmime-view-bufferコマンドを使って表示する。SEMIが必要。"
  (require 'mime-view)
  (unless (assq 'lookup-content-mode mime-preview-quitting-method-alist)
    (setq mime-preview-quitting-method-alist
	  (cons '(lookup-content-mode . ndest-mime-view-quit)
		mime-preview-quitting-method-alist)))
  (when file
    (unless (buffer-live-p ndest-mime-raw-buffer)
      (setq ndest-mime-raw-buffer (generate-new-buffer " *ndest-mime-raw*")))
    (unless (buffer-live-p ndest-mime-view-buffer)
      (setq ndest-mime-view-buffer (generate-new-buffer " *ndest-mime-view*")))
    (with-current-buffer
	ndest-mime-raw-buffer
      (widen)
      (delete-region (point-min) (point-max))
      (insert-file-contents file))
    (mime-view-buffer (buffer-name ndest-mime-raw-buffer)
		      (buffer-name ndest-mime-view-buffer))))

(defun ndest-mime-view-quit ()
  (let ((window (get-buffer-window lookup-summary-buffer)))
    (and window (select-window window)))
  (switch-to-buffer lookup-summary-buffer))

(defun ndest-follow-link-with-fiber (uri file)
  "link先の表示にfiberを起動する。"
  (start-process nil nil "fiber" (or file uri)))

(defun ndest-follow-link-with-navi2ch (_uri file)
  "link先の表示にnavi2chを利用する。"
  (require 'navi2ch)
  (require 'navi2ch-bookmark)
  (unless navi2ch-bookmark-list
    (navi2ch-bookmark-load-info))
  (if file
      (navi2ch-find-file file)
    (message "Local file is only supported.")))

(defun ndest-follow-link-with-w3m (uri file)
  "link先の表示にw3mを利用する。"
  (require 'w3m)
  (if file 
      (w3m-find-file file)
    (w3m uri)))

(defun ndest-follow-link-default (uri file)
  (if file
      (find-file file)
    (browse-url uri)))
    
;;;
;;; Arrange functions
;;;

;;(defun ndest-arrange-heading (entry)
;;  (when (and (lookup-dictionary-option dictionary ':hide-type t)
;;	     (re-search-forward "^[^:/]+/[^:]+:" nil t))
;;    (replace-match "" t t)))

(defun ndest-arrange-content (entry)
  (let* ((type (or (ndest-get-header-string "@type") ""))
	 (uri (ndest-get-header-string "@uri"))
	 params param)
    (setq params ndest-arrange-functions)
    (catch ':done
      (while params
	(setq param (car params))
	(when (and (or (null (car param))
		       (string-match (car param) type))
		   (or (null (nth 1 param))
		       (string-match (nth 1 param) uri)))
	  (funcall (nth 2 param) entry)
	  (throw ':done t))
	(setq params (cdr params)))
      (ndest-arrange-default entry))))

(defun ndest-arrange-rfc822 (entry)
  (let* ((subject (ndest-get-header-string "subject"))
	 (from (ndest-get-header-string "from"))
	 (to (ndest-get-header-string "to"))
	 (uri (ndest-get-header-string "@uri"))
	 (file (and (ndest-uri-is-file uri)
		    (ndest-get-header-string "_lreal"))))
    (search-forward "\n\n" nil t)
    (delete-region (point-min) (match-beginning 0))
    (goto-char (point-min))
    (insert (concat subject "\nFrom: " from "\nTo: " to "\n"))
    (let (start end)
      (setq start (point))
      (insert uri)
      (setq end (point))
      (ndest-set-link start end nil "message/rfc822" uri file))
    (goto-char (point-min))
    ;; (lookup-arrange-default-headings entry)
    (goto-char (point-min))
    (while (re-search-forward "\n\\([^\t\n]+\\)\t[^\n]+\n?" nil t)
      (let ((point (match-beginning 0))
	    (string (match-string 1)))
	(replace-match "" t t)
	(goto-char point)
	(insert (concat " " string " "))
	(lookup-make-region-heading point (point) 2)))
    (goto-char (point-min))
    (lookup-arrange-fill-lines entry)))

(defun ndest-arrange-navi2ch-dat (entry)
  (ndest-arrange-plain entry)
  (while (re-search-forward "<\\(br\\|\\)>" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]*>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "&\\(gt\\|lt\\|amp\\|nbsp\\);" nil t)
    (let ((string (match-string 1)))
      (replace-match 
       (lookup-assoc-get
	'(("gt"  . ">")
	  ("lt"  . "<")
	  ("amp" . "&")
	  ("nbsp" . " "))
	string) t t)))
  (goto-char (point-min))
  (lookup-arrange-fill-lines entry))

(defun ndest-arrange-default (entry)
  (ndest-arrange-plain entry)
  (goto-char (point-min)))
;;  (lookup-arrange-fill-lines entry))

(defun ndest-arrange-plain (_entry)
  (let ((uri (ndest-get-header-string "@uri"))
	(type (or (ndest-get-header-string "@type") ""))
	(file (and (ndest-uri-is-file uri)
		   (ndest-get-header-string "_lreal"))))
    (search-forward "\n\n" nil t)
    (delete-region (point-min) (match-beginning 0))
    (goto-char (point-min))
    (let (start end)
      (setq start (point))
      (if file (insert file)
        (insert uri))
      (setq end (point))
      (ndest-set-link start end nil type uri file)
      (add-text-properties start end '(read-only t)) ;; added
      ))
  (goto-char (point-min))
  (while (re-search-forward "\n\\([^\t\n]+\\)\t[^\n]+\n?" nil t)
    (let ((point (match-beginning 0))
	  (string (match-string 1)))
      (replace-match "" t t)
      (goto-char point)
      (insert (concat " " string " "))
      (lookup-make-region-heading point (point) 2))))


;;;
;;; Setup
;;;

(eval-after-load "lookup-content" '(ndest-initialize))

(when ndest-follow-link-from-entry
  (eval-after-load "lookup-entry"
    '(define-key lookup-entry-mode-map ndest-follow-link-from-entry
       (function lookup-entry-follow-ndest-link))))

(provide 'ndest)

;;; ndest.el ends here
