;;; ndic.el --- Lookup by free dictionaries
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
;;; types
;;;

(put 'ndic :arrange-table '((fill lookup-arrange-nofill)))

;; ndic agent:
;;
;;   "ndic:DIRECTORY"
;;
;; DIRECTORY - dictionary directory

;; ndic dictionary:
;;
;; NAME  - filename without directory and extension
;;
;; [property]
;; ndic-type   - `sdic' or `dictd'
;; ndic-object - [SDIC]  sdic object
;;               [dictd] the buffer of .index file
;; ndic-dict   - [dictd] .dict file name

(defun ndic-dictionary-init (dictionary)
  (let* ((dir (lookup-agent-location (lookup-dictionary-agent dictionary)))
	 (file (expand-file-name (lookup-dictionary-name dictionary) dir))
	 type object)
    (cond ((string-match "\\.sdic\\(\\.[gb]z2?\\)?\\'" file)
	   (setq type 'sdic object (ndic-sdic-init dictionary file)))
	  ((string-match "\\.dict\\(\\.dz\\)?\\'" file)
	   (setq type 'dictd object (ndic-dictd-init dictionary file)))
	  (t (error "Invalid dictionary type: %s" file)))
    (lookup-put-property dictionary 'ndic-type type)
    (lookup-put-property dictionary 'ndic-object object)))

(defun ndic-dictionary-type (dictionary)
  (or (lookup-get-property dictionary 'ndic-type)
      (progn (ndic-dictionary-init dictionary)
	     (lookup-get-property dictionary 'ndic-type))))

(defun ndic-dictionary-object (dictionary)
  (lookup-get-property dictionary 'ndic-object))

;; ndic entry:
;;
;; CODE    - [SDIC]  entry returned by sdic-search
;;           [dictd] entry position as string "START:LENGTH"
;; HEADING - headword


;;;
;;; Interface functions
;;;

(defconst ndic-dictionary-regexp
  "\\.\\(sdic\\(\\.\\(gz\\|bz2\\)\\)?\\|dict\\(\\.dz\\)?\\)\\'")

(put 'ndic :list 'ndic-list)
(defun ndic-list (agent)
  (mapcar (lambda (name) (lookup-new-dictionary agent name))
	  (directory-files (expand-file-name (lookup-agent-location agent))
			   nil ndic-dictionary-regexp)))

(put 'ndic :title 'ndic-dictionary-title)
(defun ndic-dictionary-title (dictionary)
  (let ((type (ndic-dictionary-type dictionary)))
    (cond ((eq type 'sdic) (ndic-sdic-title dictionary))
	  ((eq type 'dictd) (ndic-dictd-title dictionary)))))

(defconst ndic-sdic-methods lookup-search-methods)
(defconst ndic-dictd-methods (delq 'text lookup-search-methods))

(put 'ndic :methods 'ndic-dictionary-methods)
(defun ndic-dictionary-methods (dictionary)
  (let ((type (ndic-dictionary-type dictionary)))
    (cond ((eq type 'sdic) ndic-sdic-methods)
	  ((eq type 'dictd) ndic-dictd-methods))))

(put 'ndic :search 'ndic-dictionary-search)
(defun ndic-dictionary-search (dictionary query)
  (let ((type (ndic-dictionary-type dictionary)))
    (cond ((eq type 'sdic) (ndic-sdic-search dictionary query))
	  ((eq type 'dictd) (ndic-dictd-search dictionary query)))))

(put 'ndic :clear 'ndic-dictionary-clear)
(defun ndic-dictionary-clear (dictionary)
  (let ((type (ndic-dictionary-type dictionary))
	(obj (ndic-dictionary-object dictionary)))
    (cond ((eq type 'sdic) (sdicf-close obj))
	  ((eq type 'dictd) (kill-buffer obj)))))

(put 'ndic :content 'ndic-entry-content)
(defun ndic-entry-content (entry)
  (let ((type (ndic-dictionary-type (lookup-entry-dictionary entry))))
    (cond ((eq type 'sdic) (ndic-sdic-content entry))
	  ((eq type 'dictd) (ndic-dictd-content entry)))))


;;;
;;; SDIC
;;;

(defun ndic-sdic-init (dictionary file)
  (require 'sdicf)
  (sdicf-open file))

(defun ndic-sdic-title (dictionary)
  (let ((name (lookup-dictionary-name dictionary)))
    (if (string-match "\\.sdic\\(\\.[gb]z2?\\)?\\'" name)
	(substring name 0 (match-beginning 0)))))

(defun ndic-sdic-search (dictionary query)
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query)))
    (cond ((or (eq method 'substring) (eq method 'keyword))
	   (setq method 'text))
	  ((eq method 'wildcard)
	   (setq method 'regexp string (lookup-query-to-regexp query))))
    (mapcar (lambda (entry)
	      (lookup-new-entry 'regular dictionary entry
				(sdicf-entry-headword entry)))
	    (sdicf-search (ndic-dictionary-object dictionary) method string))))

(defun ndic-sdic-content (entry)
  (setq entry (lookup-entry-code entry))
  (format "%s\n\n    %s\n"
	  (sdicf-entry-headword entry) (sdicf-entry-text entry)))


;;;
;;; dictd
;;;

(defun ndic-dictd-init (dictionary file)
  (let* ((index (concat ;; remove ".dz.index"
                 (file-name-sans-extension
                  (file-name-sans-extension file)) ".index"))
	 (buffer (get-buffer-create (concat " *ndic " index "*"))))
    (with-current-buffer buffer (insert-file-contents index))
    (unless (file-exists-p index)
      (error "No .index file for `%s'" index))
    (lookup-put-property dictionary 'ndic-dict file)
    buffer))

(defun ndic-dictd-title (dictionary)
  (let* ((query (lookup-new-query 'exact "00-database-short"))
	 (title (ndic-dictd-search dictionary query)))
    (when title
      (setq title (ndic-dictd-content (car title)))
      (if (string-match "\n *\\(.*\\)\n" title)
	  (setq title (match-string 1 title)))
      title)))

(defun ndic-dictd-search (dictionary query)
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
      (with-current-buffer (ndic-dictionary-object dictionary)
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

(defun ndic-dictd-content (entry)
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
