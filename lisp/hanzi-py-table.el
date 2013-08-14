;;; hanzi-py-table.el --- Hanzi to Pinyin (simple form) table  -*- lexical-binding: t -*-

;; Copyright (c) KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; URL: http://github.com/kawabata/

;;; Commentary:

;; `hanzi-py-table' provides simple hanzi to pinyin table mainly for
;; dictionary purpose.  This table is extracted from quail files for
;; pinyin input in Emacs leim packages.

;;; Code:

(eval-when-compile
  (defun hanzi-py-table-build-internal (elt key)
    (let ((target (nth 1 elt)))
      (setq key (concat key (char-to-string (car elt))))
      (apply
       'nconc
       (cond 
	((integerp target)
	 (list (cons key (list target))))
	(target
	 (list
	  (cons key
		(delq nil
		      (mapcar
		       (lambda (elt) (when (stringp elt)
				       (string-to-char elt)))
		       (if (consp target) (cdr target) target)))))))
       (mapcar (lambda (elt)
		 (hanzi-py-table-build-internal elt key))
	       (cddr elt))))))

(defvar hanzi-py-table
  (let ((table (make-char-table 'char-code-property-table))
	(alist
	 (eval-when-compile
	   (apply
	    'nconc
	    (mapcar
	     (lambda (method)
	       (let ((src (locate-library (concat (cadr method) ".el"))))
		 (if src
		     (with-temp-buffer 
		       (message "Reading file %s" src)
		       (insert-file-contents src)
		       (re-search-forward "(quail-define-rules")
		       (mapcar (lambda (elt)
				 (cons (car elt) (string-to-list (cadr elt))))
			       (cdar (read-from-string
				      (buffer-substring (match-beginning 0)
							(point-max))))))
		   (unwind-protect
		       (progn
			 (apply 'quail-use-package method)
			 (apply 'nconc
				(mapcar
				 (lambda (elt)
				   (when elt
				     (hanzi-py-table-build-internal elt nil)))
				 (nth 2 (quail-package (car method))))))
		     (quail-deactivate)))))
	     '(("chinese-tonepy" "quail/TONEPY")
	       ("chinese-py-b5" "quail/PY-b5")))))))
    (dolist (item alist)
      (let ((py (car item)) (charlist (cdr item)))
	(dolist (char charlist)
	  (aset table char (push py (aref table char))))))
    (map-char-table (lambda (k v) (setq v (delete-dups v))
		      (when (= 1 (length v)) (aset table k (car v))))
		    table)
    table))

(set-char-table-extra-slot hanzi-py-table 0 'hanzi-pinyin)
(define-char-code-property 'hanzi-pinyin hanzi-py-table)

(provide 'hanzi-py-table)
