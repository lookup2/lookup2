;;; ndkks.el --- Lookup KAKASI interface
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Keywords: dictionary

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

;;;
;;; Customizable variables
;;;

(defgroup ndkks nil
  "Lookup KAKASI interface."
  :group 'lookup-search-agents)

(defcustom ndkks-program-name "kakasi"
  "*KAKASI のプログラム名。"
  :type 'string
  :group 'ndkks)

(defcustom ndkks-dictionary-title "漢字かな辞典"
  "*ndkks 辞書のタイトル。"
  :type 'string
  :group 'ndkks)

(defcustom ndkks-content-format
  '(t "\n" ("-JH") "\n" "【振り仮名】" ("-JH" "-f" "-p") "\n")
  "*ndkks 辞書が出力するエントリ内容のフォーマット。
リストの各要素として次のものを指定出来る。
`文字列' - それがそのまま挿入される。
`リスト' - それを引数として KAKASI が呼び出され、結果が挿入される。
`t'      - 検索語が挿入される。"
  :type '(repeat (radio :tag "出力内容"
			(const :tag "検索語" t)
			(string :tag "文字列")
			(repeat :tag "KAKASI 呼び出し"
				(string :tag "option"))))
  :group 'ndkks)

(defcustom ndkks-process-coding-system lookup-kakasi-coding-system
  "*Coding system for KAKASI process."
  :type 'symbol
  :group 'ndkks)


;;;
;;; Internal variables
;;;

(if (string-match "^19" emacs-version) 
    (defconst ndkks-valid-charsets (list lc-jp))
  (defconst ndkks-valid-charsets '(japanese-jisx0208)))

(put 'ndkks ':methods '(exact))

;;;
;;; Interface functions
;;;

(put 'ndkks ':list 'ndkks-list)
(defun ndkks-list (agent)
  (unless (featurep 'mule)
    (error "ndkks requires `mule' feauture."))
  (call-process ndkks-program-name nil 0)	; check KAKASI exists
  (list (lookup-new-dictionary agent ndkks-program-name)))

(put 'ndkks ':title ndkks-dictionary-title)

(put 'ndkks ':clear 'ndkks-clear)
(defun ndkks-clear (agent)
  (ndkks-process-kill))

(put 'ndkks ':search 'ndkks-dictionary-search)
(defun ndkks-dictionary-search (dictionary query)
  (let ((string (lookup-query-string query)))
    (and
     ;; xxx: 漢字が含まれているか調べたいのだが、どうやればいいんだろう?
     ;; xxx: とりあえず、適当にチェック。
     (let ((charsets (find-charset-string string)))
       (catch 'return
	 (while charsets
	   (if (memq (car charsets) ndkks-valid-charsets)
	       (throw 'return t)
	     (setq charsets (cdr charsets))))))
     (string-match "[^あ-んア-ンーＡ-Ｚａ-ｚ]" string)
     (list (lookup-new-entry 'regular dictionary string)))))

(put 'ndkks ':content 'ndkks-entry-content)
(defun ndkks-entry-content (entry)
  (let ((string (lookup-entry-code entry)))
    (mapconcat (lambda (element)
		 (cond ((eq element t) string)
		       ((stringp element) element)
		       ((listp element) (ndkks-process-require element string))
		       (t (error "Invalid format element: %S" element))))
	       ndkks-content-format "")))


;;;
;;; KAKASI process
;;;

(defvar ndkks-process-alist nil)

(defun ndkks-get-process (args)
  (let ((process (lookup-assoc-get ndkks-process-alist args)))
    (unless (and process (eq (process-status process) 'run))
      (if process (kill-process process))
      (let ((buffer (lookup-open-process-buffer " *ndkks*")))
	(setq process (apply 'start-process "ndkks" buffer
			     ndkks-program-name args))
	(process-kill-without-query process)
	;; 起動後、少し時間を置かないと、最初の検索がうまくいかない。
	(sleep-for 0.1)
	(let ((coding ndkks-process-coding-system))
	  (when coding
	    (set-process-coding-system process coding coding)))
	(setq ndkks-process-alist
	      (lookup-assoc-put ndkks-process-alist args process))))
    process))

(defun ndkks-process-require (args string)
  (lookup-process-require (ndkks-get-process args) (concat string "\n") "\n"))

(defun ndkks-process-kill ()
  (while ndkks-process-alist
    (lookup-process-kill (cdar ndkks-process-alist))
    (setq ndkks-process-alist (cdr ndkks-process-alist))))

(provide 'ndkks)

;;; ndkks.el ends here
