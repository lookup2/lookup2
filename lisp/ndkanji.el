;;; ndkanji.el --- Lookup `Kanji' interface -*- coding: utf-8 -*-
;; Copyright (C) 2009 KAWABATA Taichi <kawabata.taichi@gmail.com>

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

;;; Documentation:

;; ndkanji.el provides the Kanji IDS searching capabilities.
;; IDS data is provided at the following URL:
;;
;; http://kanji-database.sourceforge.net/ids/ids.html
;;
;; This agent uses `sqlite3' program for searching the Kanji Data.
;; You must prepare Kanji Database `kdp.sl3' and `unihan.sl3' file.  
;;
;; http://kanji-database.sourceforge.net/database/index.html.

;;; Usage:
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndkanji "~/edicts/kdp/") ; directory where kdp.sl3 and unihan.sl3 data file exists.
;;           ....
;;           ))

;;; Code:

(require 'lookup)
(require 'lookup-utils)



;;;
;;; Customizable Variables
;;;

(defvar ndkanji-sl3-program "env")
(defvar ndkanji-sl3-program-options '("LANG=ja_JP.utf-8" "sqlite3"))
(defvar ndkanji-sl3-prompt "sqlite>")
(defvar ndkanji-sl3-kdp "kdp.sl3")
(defvar ndkanji-sl3-unihan "unihan.sl3")

;;;
;;; Internal Variables
;;;

(defvar ndkanji-kdp-sl3 
  (expand-file-name "~/edicts/KDP/kdp.sl3"))

(defvar ndkanji-unihan-sl3
  (expand-file-name "~/edicts/KDP/unihan.sl3"))

(defvar ndkanji-sql-utf8-format 
  "select distinct ch.v from ch where ch.k in (%s) limit 200")

(defvar ndkanji-sql-ucs-format 
  "select distinct * from (%s) limit 200")

(defvar ndkanji-sql-count-format 
  "select count(*) from (%s)")

(defvar ndkanji-sql-ids-format 
  "select ids.k from ids where ids.v glob \"%s\"")

(defvar ndkanji-sql-strokes-format 
  "select strokes.k from strokes where strokes.v between %d and %d ")

(defvar ndkanji-sql-japanese-format
  "select unihan.kIRG_JSource.k from unihan.kIRG_JSource where unihan.kIRG_JSource.v is not null ")

(defvar ndkanji-sql-char-strokes-format
  "select strokes.v from strokes where strokes.k = \"%X\" ")

(defvar ndkanji-sql-char-ids-format
  "select ids.v from ids where ids.k in (select ch.k from ch where ch.v = \"%c\") ")

(defvar ndkanji-sql-similar-format
  "select ch.v from ch where ch.k in (select similar.k from similar where similar.v = \"%s\" union select similar.v from similar where similar.k = \"%s\")")

(defvar ndkanji-parse-regexp
  "\\([?*⿰-⿻⺀-⻳〢㇀㇉㐀-鿿豈-﫿𠀀-𫜴-]+\\)\\([0-9][0-9]?\\)?\\(-[0-9]?[0-9]?\\)?\\(J\\)?")

(defvar ndkanji-kanji-regexp "[⺀-⻳〢㇀㇉㐀-鿿豈-﫿𠀀-𫜴-]")

;;;
;;; types
;;;

(put 'ndkanji :methods '(exact))
(put 'ndkanji :priority 'secondary)

;;;
;;; Interface functions
;;;

(put 'ndkanji :list 'ndkanji-list)
(defun ndkanji-list (agent)
  (list (lookup-new-dictionary agent "")))

(put 'ndkanji :title 'ndkanji-title)
(defun ndkanji-title (dictionary)
  (or (lookup-dictionary-option dictionary :title)
      "漢字部品検索"))

(put 'ndkanji :search 'ndkanji-search)
(defun ndkanji-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (if (null ndkanji-kdp-sl3)
      (ndkanji-sl3-setup 
       (lookup-agent-location 
        (lookup-dictionary-agent dictionary))))
  (let* ((query (lookup-query-string query))
         (result (ndkanji-sl3-query 
                  (ndkanji-construct-sql query))))
    (mapcar (lambda (ucs)
              (lookup-new-entry 'dynamic dictionary ucs))
            result)))

(put 'ndkanji :dynamic 'ndkanji-dynamic-search)
(defun ndkanji-dynamic-search (entry)
  "Return newly searched entries for ENTRY."
  (lookup-with-message "Rechecking"
    (let* ((module (lookup-current-module))
	   (self (lookup-entry-dictionary entry))
	   (word (lookup-entry-code entry))
	   (query (lookup-new-query 'default word))
	   prio entries search-found)
      (setq entries
	    (mapcar (lambda (dict)
		      (setq prio (lookup-module-dictionary-priority module dict))
		      (when (and (not (eq dict self))
				 (cond ((eq prio t) t)
				       ((eq prio 'secondary) (not search-found))
				       ((eq prio 'supplement) search-found)))
			(lookup-message
			 (format "by %s..." (lookup-dictionary-title dict)))
			(setq entries (lookup-dictionary-search dict query))
			(if entries (setq search-found t))
			entries))
		    (lookup-module-dictionaries module)))
      (setq entries (remove-if 
                     (lambda (x) (equal (lookup-entry-type x) 'dynamic))
                     (apply 'append entries)))
      (setq entries (mapcar 'lookup-new-slink entries))
      entries)))

;;;
;;; Process Function
;;;

(defun ndkanji-sl3-query (query)
  "Return the result of the list by the end."
  (when (null ndkanji-kdp-sl3) (error "Initialization Failed!"))
  (lookup-with-coding-system 'utf-8
    (let* ((command-args (append (list ndkanji-sl3-program) 
                                 ndkanji-sl3-program-options
                                 (list ndkanji-kdp-sl3)))
           (output (lookup-get-process-require 
                    command-args (concat query ";")
                    ndkanji-sl3-prompt)))
      (split-string output "\n" t))))

(defun ndkanji-sl3-exit ()
  (let ((command-args (list ndkanji-sl3-program ndkanji-kdp-sl3)))
    (lookup-get-process-kill command-args)))

(defun ndkanji-sl3-setup (location)
  (setq ndkanji-unihan-sl3 (expand-file-name "unihan.sl3" location)
        ndkanji-kdp-sl3    (expand-file-name "kdp.sl3" location))
  (if (file-exists-p ndkanji-unihan-sl3)
      (ndkanji-sl3-query 
       (concat "attach \"" ndkanji-unihan-sl3 "\" as unihan"))))

(defun ndkanji-sl3-char-strokes (char)
  "Get strokes of specified CHAR."
  (let ((output
         (ndkanji-sl3-query 
          (format ndkanji-sql-char-strokes-format char))))
    (mapcar 'string-to-number output)))

(defun ndkanji-sl3-char-ids (char)
  "Get IDS of specified CHAR."
  (let ((output
         (ndkanji-sl3-query 
          (format ndkanji-sql-char-ids-format char))))
    (mapconcat
     'identity
     (split-string (regexp-opt output t) "[\\():?]" t) "")))

;;;
;;; Strokes Calculation Function
;;;

(defun ndkanji-mapthread (function seq1 &rest seqrest)
  ;; ind-util.el にはバグあり。
  (if seqrest
      (mapcar 
       (lambda (x)
         (apply 
          'ndkanji-mapthread 
          `(lambda (&rest y) (apply (quote ,function) ,x y))
          seqrest))
       seq1)
  (mapcar function seq1)))

(defun ndkanji-strokes-ids (ids)
  "Calculate total strokes of IDS."
  (when (null (string-match "？" ids))
    (let* ((chars (string-to-list (replace-regexp-in-string "[^㐀-鿆𠀀-𫜴]" "" ids)))
           (chars-uniq (remove-duplicates chars))
           ;; IDS中の各漢字の数を数える。
           (char-count (mapcar (lambda (x) (cons x (count x chars))) chars-uniq))
           ;; (漢字 . 漢字数) を、(画数リスト . 漢字数) に変換。
           (strokes-count (mapcar (lambda (x) (cons (ndkanji-sl3-char-strokes (car x))
                                                    (cdr x)))
                                  char-count))
           ;; 掛け算により、画数リストに変換。
           (strokes (mapcar (lambda (x)
                              (mapcar (lambda (y) (* (cdr x) y)) (car x)))
                            strokes-count))
           ;; 各画数リストを足しあわせる。
           (strokes-list (flatten (apply 'ndkanji-mapthread '+ strokes))))
      strokes-list)))

;;;
;;; char-to-code, code-to-char functions
;;;

(defun kdp-util-char-to-code (char)
  (if (and (< #xf100 char) (< char #xf700))
      (let* ((x (- char #xee1b))
             (y (% x 157))
             (y (+ y (if (< y 63) 64 98))))
        (format "CDP-%02X%02X" (+ (/ x 157) #x80) y))
    (format "%X" char)))

(defun kdp-util-code-to-char (code)
  (if (string-match "^CDP" code)
      (let* ((x (string-to-number (substring str 4 6) 16))
             (y (string-to-number (substring str 6 8) 16))
             (x (+ (* (- x #x80) 157) (if (< y 129) (- y 64) (- y 98)))))
        (+ x #xee1b))
    (string-to-number code 16)))

;;;
;;; Main Function
;;;

(defun ndkanji-construct-sql (query &optional count)
  "Construct QUERY string.
It returns a list of `ids', `stroke range' and `option'.
If it does not match, return nil."
  (when (string-match (concat "^" ndkanji-parse-regexp "$") query)
    (let* ((ids (match-string 1 query))
           (from (match-string 2 query))
           (to (match-string 3 query))
           (flag (match-string 4 query))
           strokes
           ids-sql
           strokes-sql
           flag-sql)
      (setq strokes (if (or from to) (ndkanji-strokes-ids ids))
            from    (if from (string-to-number from) 0)
            to      (if to (if (= (length to) 1) 99 ;; e.g. "20-" -> "20-99"
                             (- 0 (string-to-number to)))
                      from)
            ids-sql (format ndkanji-sql-ids-format (concat "*" ids "*")))
      (when strokes
        (setq strokes-sql
              (mapconcat (lambda (x)
                           (format ndkanji-sql-strokes-format (+ x from) (+ x to)))
                         strokes " intersect ")))
      (when flag 
        (setq flag-sql ndkanji-sql-japanese-format))
      (format 
       (if count ndkanji-sql-count-format
           ndkanji-sql-utf8-format )
       (concat ids-sql
               (if strokes-sql (concat " intersect " strokes-sql))
               (if flag (concat " intersect " flag-sql))))
      )))

;;;
;;; Ambiguous Expansion Function
;;;
(defun ndkanji-expand-wildcard (str)
  (let ((chars (string-to-list str)))
    (mapconcat (lambda (ch)
                 (let ((chars (ndkanji-sl3-query 
                               (format ndkanji-sql-similar-format 
                                       (kdp-util-char-to-code char)
                                       (kdp-util-char-to-code char)))))
                   (if chars (concat "[" chars "]") ch)))
               "")))

;;;
;;; Edit Function
;;;

(defun ndkanji-edit-char (arg)
  "Edit Kanji Components.
If ARG is provided, decompose char even if char exists at point."
  (interactive "P")
  (cond
   ((and (null arg)
         (looking-at ndkanji-parse-regexp))
    ;; Forward matching
    (let ((end (match-end 0))
          (result (ndkanji-sl3-query (ndkanji-construct-sql (match-string 0)))))
      (if (null result)
          (message "No such pattern!")
        (delete-region (point) end)
        (if (< (length result) 2)
            (apply 'insert result)
          (insert "《")
          (apply 'insert result)
          (insert "》")))))
   ((looking-back ndkanji-kanji-regexp)
    (let* ((char (char-before (point)))
           (decomposition (ndkanji-sl3-char-ids char)))
      (when decomposition
        (backward-delete-char 1)
        (insert decomposition))))))

(global-set-key "\M-U" 'ndkanji-edit-char)

(provide 'ndkanji)

;;; ndkanji.el ends here
