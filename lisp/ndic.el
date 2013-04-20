;;; ndic.el --- Lookup by `dict'-format dictionaries -*- lexical-binding: t -*-
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>
;; Copyright (C) 2009 Lookup Development Team

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
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

;;; Documentation

;; `ndic' agent provides an ability to access `dictd'-based dictionaries.
;; Tools for `dictd' dictionaries are distributed from http://www.dict.org/.
;; Some internet dictionaries, such as the ones distributed by `freedict'
;; project, are based on this format.
;;
;; `dict' dictionaries may be distributed in either compressed
;; (`**.dict.dz' extension) or non-compressed (`**.dict') format, AND
;; index file (`**.index') in either case.  You need `dictzip' tool
;; installed on your system to use compressed format.
;;
;; Most Linux distributions have packages for `dictzip' programs.  
;; You can download the source code from `ftp://ftpdict.org/dict/'.
;; 
;; This agent no longer supports `sdic' format.  (Use `ndsary' or etc. instead)

;;; Code:

(require 'lookup)

;;;
;;; Customizable variables
;;;

(defgroup ndic nil
  "Lookup by free dictionaries."
  :group 'lookup-search-agents)

(defcustom ndic-dictzip-program "dictzip"
  "*Program name of dictzip."
  :type 'string
  :group 'ndic)

(defvar ndic-coding-system 'utf-8)


;;;
;;; Arrange table
;;;

(put 'ndic :arrange-table '((fill lookup-arrange-nofill)))


;;;
;;; Interface functions
;;;

(defconst ndic-dictionary-regexp "\\.\\(dict\\(\\.dz\\)?\\)\\'")

(defconst ndic-dictionary-uncompressed-regexp "\\.dict\\'")

(put 'ndic :list 'ndic-list)
(defun ndic-list (agent)
  (let ((dir (lookup-agent-location agent)))
    (if (file-directory-p dir)
        (mapcar (lambda (name) (lookup-new-dictionary agent name))
                (ndic-directory-files (lookup-agent-location agent)))
      (error "ndic: directory %s is not found." dir)
      nil)))

(put 'ndic :title 'ndic-dictionary-title)
(defun ndic-dictionary-title (dictionary)
  (let* ((query (lookup-new-query 'exact "00-database-short"))
	 (title (ndic-dictionary-search dictionary query)))
    (when title
      (setq title (ndic-entry-content (car title)))
      (if (string-match "\n *\\(.*\\)\n" title)
	  (setq title (match-string 1 title)))
      title)))

(defconst ndic-dictd-methods (remove 'text lookup-search-methods))

(put 'ndic :methods 'ndic-dictionary-methods)
(defun ndic-dictionary-methods (_dictionary)
  ndic-dictd-methods)

(put 'ndic :search 'ndic-dictionary-search)
(defun ndic-dictionary-search (dictionary query)
  (let* ((method (lookup-query-method query))
	 (string (lookup-query-string query))
	 (regexp (regexp-quote string)))
    (setq regexp
	  (cond ((eq method 'exact) (concat "^" regexp "\t"))
		((eq method 'prefix) (concat "^[^\t]*\\<" regexp))
		((eq method 'suffix) (concat "^[^\t]*" regexp "\\>"))
		((eq method 'substring) (concat "^[^\t]*" regexp))
		((eq method 'wildcard) (lookup-query-to-regexp query))
		((eq method 'regexp) string)
		((eq method 'keyword) (concat "^[^\t]*\\<" regexp "\\>"))))
    (let ((case-fold-search t) code entries)
      (with-current-buffer (ndic-dictionary-buffer dictionary)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
          (goto-char (line-beginning-position))
	  (when (looking-at "\\([^\t\n]+\\)\t\\([^\t\n]+\\)\t\\(.+\\)")
            (setq string (match-string 1))
	    (setq code (concat (match-string 2) ":" (match-string 3)))
	    (setq entries
		  (cons (lookup-new-entry 'regular dictionary code string)
			entries)))
          (goto-char (line-end-position)))
	(nreverse entries)))))

(put 'ndic :clear 'ndic-dictionary-clear)
(defun ndic-dictionary-clear (dictionary)
  (let ((obj (ndic-dictionary-buffer dictionary)))
    (kill-buffer obj)))

(put 'ndic :content 'ndic-entry-content)
(defun ndic-entry-content (entry)
  (let ((code (lookup-entry-code (lookup-entry-substance entry)))
	(dict (lookup-get-property
	       (lookup-entry-dictionary entry) 'ndic-dict)))
    (if (null (string-match ":" code))
        (error "ndic internal error.")
      (setq code (cons (substring code 0 (match-beginning 0))
                       (substring code (match-end 0))))
      (lookup-with-coding-system ndic-coding-system
        (with-temp-buffer
          (if (string-match "\\.dz$" dict)
              (call-process ndic-dictzip-program nil t nil
                            "-cdk" "-S" (car code) "-E" (cdr code) dict)
            (let ((offset (ndic-b64-decode (car code)))
                  (length (ndic-b64-decode (cdr code))))
              (insert-file-contents dict nil offset (+ offset length))))
          (buffer-string))))))


;;;
;;; dictd
;;;

(defun ndic-dictionary-buffer (dictionary)
  (unless (lookup-get-property dictionary 'ndic-buffer)
    (ndic-dictionary-init dictionary))
  (lookup-get-property dictionary 'ndic-buffer))

(defun ndic-dictionary-init (dictionary)
  "Initialize DICTIONARY.  Set up file and buffer."
  (let* ((dir (lookup-agent-location (lookup-dictionary-agent dictionary)))
	 (file (expand-file-name (lookup-dictionary-name dictionary) dir))
         (index (ndic-index-file file))
         (buffer (get-buffer-create (concat " *ndic "index "*"))))
    (with-current-buffer buffer (insert-file-contents index))
    (lookup-put-property dictionary 'ndic-dict file)
    (lookup-put-property dictionary 'ndic-buffer buffer)))

(defun ndic-directory-files (dir)
  "Get valid `dict' files in the directory DIR."
  (let ((dicts
         (directory-files
          dir nil (if (executable-find ndic-dictzip-program)
                      ndic-dictionary-regexp
                    ndic-dictionary-uncompressed-regexp)))
        valid-dicts)
    (dolist (dict dicts)
      (if (file-exists-p
           (expand-file-name (ndic-index-file dict) dir))
          (setq valid-dicts (cons dict valid-dicts))))
    (nreverse valid-dicts)))

(defun ndic-index-file (original-file)
  "Get corresponding index file of ORIGINAL-FILE."
  (concat (file-name-sans-extension ; remove .dict.dz
           (file-name-sans-extension original-file)) ".index"))

(defconst ndic-b64-table
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun ndic-b64-decode (code)
  (let ((n 0) (len (length code)) (value 0)
	(case-fold-search nil))
    (while (< n len)
      (if (string-match (substring code n (1+ n)) ndic-b64-table)
	  (setq value (+ (* value 64) (match-beginning 0))))
      (setq n (1+ n)))
    value))

(provide 'ndic)

;;; ndic.el ends here
