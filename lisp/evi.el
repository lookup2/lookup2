;;; evi.el --- Emacs version integrator
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

(or (fboundp 'when)
    (defmacro when (cond &rest body) (` (if (, cond) (progn (,@ body))))))
(or (fboundp 'unless)
    (defmacro unless (cond &rest body) (` (if (, cond) nil (,@ body)))))

(or (fboundp 'caar) (defun caar (obj) (car (car obj))))
(or (fboundp 'cadr) (defun cadr (obj) (car (cdr obj))))
(or (fboundp 'cdar) (defun cdar (obj) (cdr (car obj))))
(or (fboundp 'cddr) (defun cddr (obj) (cdr (cdr obj))))

(or (fboundp 'defgroup)
    (defmacro defgroup (symbol members doc &rest rest) nil))
(or (fboundp 'defcustom)
    (defmacro defcustom (symbol value doc &rest rest)
      (list 'defvar symbol value doc)))
(or (fboundp 'defface)
    (defmacro defface (face spec doc &rest rest)
      (let ((list (cadr (assq t (eval spec))))
	    (symbol (list 'quote face))
	    key value exp)
	(while list
	  (setq key (car list) value (cadr list))
	  (setq exp (cons (cond ((eq key ':bold)
				 (list 'set-face-bold-p symbol value))
				((eq key ':underline)
				 (list 'set-face-underline-p symbol value))
				((eq key ':foreground)
				 (list 'set-face-foreground symbol value)))
			  exp))
	  (setq list (cddr list)))
	(cons 'progn (cons (list 'setq face (list 'make-face symbol)) exp)))))

(or (fboundp 'plist-get)
    (defun plist-get (plist prop)
      (catch 'value
	(while plist
	  (if (eq (car plist) prop)
	      (throw 'value (cadr plist))
	    (setq plist (cddr plist)))))))

(or (fboundp 'plist-put)
    (defun plist-put (plist prop val)
      (catch 'new-list
	(let ((list plist))
	  (while list
	    (when (eq (car list) prop)
	      (setcar (cdr list) val)
	      (throw 'new-list plist))
	    (setq list (cddr list)))
	  (cons prop (cons val plist))))))

(or (fboundp 'save-current-buffer)
    (defmacro save-current-buffer (&rest body)
      (` (let ((evi-orig-buffer (current-buffer)))
	   (unwind-protect
	       (progn (,@ body))
	     (set-buffer evi-orig-buffer))))))

(or (fboundp 'save-selected-window)
    (defmacro save-selected-window (&rest body)
      (` (let ((save-selected-window-window (selected-window)))
	   (unwind-protect
	       (progn (,@ body))
	     (select-window save-selected-window-window))))))

(or (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest body)
      (` (save-current-buffer
	   (set-buffer (, buffer))
	   (,@ body)))))

(or (fboundp 'with-temp-buffer)
    (defmacro with-temp-buffer (&rest forms)
      (let ((temp-buffer (make-symbol "temp-buffer")))
	(` (let (((, temp-buffer)
		  (get-buffer-create (generate-new-buffer-name " *temp*"))))
	     (unwind-protect
		 (with-current-buffer (, temp-buffer)
		   (,@ forms))
	       (and (buffer-name (, temp-buffer))
		    (kill-buffer (, temp-buffer)))))))))

(or (fboundp 'with-temp-file)
    (defmacro with-temp-file (file &rest forms)
      (let ((temp-file (make-symbol "temp-file"))
	    (temp-buffer (make-symbol "temp-buffer")))
	(` (let (((, temp-file) (, file))
		 ((, temp-buffer) (get-buffer-create
				   (generate-new-buffer-name " *temp file*"))))
	     (unwind-protect
		 (prog1 (with-current-buffer (, temp-buffer) (,@ forms))
		   (with-current-buffer (, temp-buffer)
		     (widen)
		     (write-region (point-min) (point-max)
				   (, temp-file) nil 0)))
	       (and (buffer-name (, temp-buffer))
		    (kill-buffer (, temp-buffer)))))))))

(or (fboundp 'add-to-list)
    (defun add-to-list (symbol element)
      (or (member element (symbol-value symbol))
	  (set symbol (cons element (symbol-value symbol))))))

(or (fboundp 'match-string)
    (defun match-string (num &optional string)
      (if (match-beginning num)
	  (if string
	      (substring string (match-beginning num) (match-end num))
	    (buffer-substring (match-beginning num) (match-end num))))))

(or (fboundp 'buffer-substring-no-properties)
    (defun buffer-substring-no-properties (start end)
      (format "%s" (buffer-substring start end))))

(or (fboundp 'file-name-sans-extension)
    (defun file-name-sans-extension (filename)
      (if (string-match "\\.[^./]*$" filename)
	  (substring filename 0 (match-beginning 0))
	filename)))

(or (fboundp 'string-width) (defalias 'string-width 'length))
(or (fboundp 'string-to-list) (defalias 'string-to-list 'string-to-char-list))
(or (fboundp 'make-char) (defalias 'make-char 'make-character))
(or (fboundp 'buffer-live-p) (defalias 'buffer-live-p 'buffer-name))

(or (fboundp 'frame-first-window)
    (defalias 'frame-first-window 'frame-highest-window))

(or (fboundp 'make-variable-frame-local)
    (defalias 'make-variable-frame-local 'make-variable-buffer-local))

(or (fboundp 'frame-parameter)
    (if (fboundp 'frame-property)
        (defalias 'frame-parameter 'frame-property)
      (defun frame-parameter (frame parameter)
        (cdr (assq parameter (frame-parameters frame))))))

(when (string< emacs-version "20")
  (defvar evi-orig-char-after (symbol-function 'char-after))
  (defun char-after (&optional pos)
    (funcall evi-orig-char-after (or pos (point))))

  (defvar evi-orig-replace-match (symbol-function 'replace-match))
  (defun replace-match (newtext &optional fixedcase literal string)
    (if (not string)
	(funcall evi-orig-replace-match newtext fixedcase literal)
      (with-temp-buffer
	(insert string)
	(set-match-data (mapcar '1+ (match-data)))
	(funcall evi-orig-replace-match newtext fixedcase literal)
	(buffer-substring (point-min) (point-max)))))
  )


(when (or (string< emacs-version "20") (featurep 'xemacs))
  (defvar evi-orig-read-string (symbol-function 'read-string))
  (defun read-string (prompt &optional initial history default inherit)
    (let ((input (funcall evi-orig-read-string prompt initial)))
      (if (and default (equal input ""))
	  default
	input)))

  (defvar evi-orig-completing-read (symbol-function 'completing-read))
  (defun completing-read (prompt table &optional predicate require-match
				 initial histry default inherit)
    (let ((input (funcall evi-orig-completing-read prompt table predicate
			  require-match initial histry)))
      (if (and default (equal input ""))
	  default
	input)))

  (when (string< emacs-version "20.3")
    (defvar evi-orig-string-to-number (symbol-function 'string-to-number))
    (defun string-to-number (string &optional base)
      (if (not base)
	  (funcall evi-orig-string-to-number string)
	(let ((len (length string))
	      (number 0) (i 0) c)
	  (if (or (< base 2) (< 16 base))
	      (error "Args out of range: %d" base)
	    (while (< i len)
	      (setq number (* number base))
	      (setq c (aref string i))
	      (cond
	       ((and (<= ?0 c) (<= c ?9)) (setq number (+ number (- c ?0))))
	       ((and (<= ?a c) (<= c ?f)) (setq number (+ number (- c ?a -10))))
	       ((and (<= ?A c) (<= c ?F)) (setq number (+ number (- c ?A -10))))
	       (t (setq i len)))
	      (setq i (1+ i)))
	    number)))))
  )

(if (featurep 'mule) (require 'evi-mule))

(provide 'evi)

;;; evi.el ends here
