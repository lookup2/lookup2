;;; lookup-record.el --- search record for statistics
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lookup Record Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun lookup-record-display ()
  (interactive)
  (with-current-buffer (lookup-open-buffer (lookup-record-buffer))
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Dictionary Used:\n\n")
      (lookup-record-insert-dictionary 'count-used)
      (insert "\nEntry Found:\n\n")
      (lookup-record-insert-entry 'count-found)
      (insert "\nEntry Displayed:\n\n")
      (lookup-record-insert-entry 'count-displayed)
      (insert "\nGaiji Loaded:\n\n")
      (lookup-record-insert-gaiji 'count-loaded)
      (insert "\nGaiji Inserted:\n\n")
      (lookup-record-insert-gaiji 'count-inserted))
    (goto-char (point-min))
    (if (window-live-p lookup-start-window)
	  (set-window-buffer lookup-start-window (current-buffer))
	(display-buffer (current-buffer)))))

(defun lookup-record-insert-dictionary (prop)
  (lookup-foreach
   (lambda (pair)
     (insert (format "%5d: %s\n" (cdr pair)
		     (lookup-dictionary-title (car pair)))))
   (sort (lookup-grep 'cdr (mapcar (lambda (pair)
				     (cons (cdr pair)
					   (lookup-dictionary-get-property
					    (cdr pair) prop)))
				   (lookup-dictionary-alist)))
	 (lambda (e1 e2) (> (cdr e1) (cdr e2))))))

(defun lookup-record-insert-entry (prop)
  (lookup-foreach
   (lambda (pair)
     (insert (format "%5d: " (cdr pair))
	     (lookup-dictionary-head
	      (lookup-entry-dictionary (car pair))) " \n")
     (backward-char)
     (lookup-entry-heading-insert (car pair))
     (forward-char))
   (sort (lookup-grep 'cdr (mapcar (lambda (entry)
				     (cons entry (lookup-entry-get-property
						  entry prop)))
				   (lookup-entry-list)))
	 (lambda (e1 e2) (> (cdr e1) (cdr e2))))))

(defun lookup-record-insert-gaiji (prop)
  (lookup-foreach
   (lambda (pair)
     (insert (format "%5d: " (cdr pair)))
     (lookup-gaiji-insert (car pair))
     (insert "\n"))
   (sort (lookup-grep 'cdr (mapcar (lambda (gaiji)
				     (cons gaiji (lookup-gaiji-get-property
						  gaiji prop)))
				   (lookup-gaiji-list)))
	 (lambda (e1 e2) (> (cdr e1) (cdr e2))))))

;;;
;;; Record functions
;;;

;;;###autoload
(defun lookup-record-dictionary-used (dictionary)
  (let ((count (lookup-dictionary-get-property dictionary 'count-used)))
    (setq count (if count (1+ count) 1))
    (lookup-dictionary-put-property dictionary 'count-used count)))

(defun lookup-record-entry-found (entry)
  (when (setq entry (lookup-entry-substance entry))
    (let ((count (lookup-entry-get-property entry 'count-found)))
      (setq count (if count (1+ count) 1))
      (lookup-entry-put-property entry 'count-found count))))

(defun lookup-record-entry-displayed (entry)
  (when (setq entry (lookup-entry-substance entry))
    (let ((count (lookup-entry-get-property entry 'count-displayed)))
      (setq count (if count (1+ count) 1))
      (lookup-entry-put-property entry 'count-displayed count))))

(defun lookup-record-gaiji-loaded (gaiji)
  (let ((count (lookup-gaiji-get-property gaiji 'count-loaded)))
    (setq count (if count (1+ count) 1))
    (lookup-gaiji-put-property gaiji 'count-loaded count)))

(defun lookup-record-gaiji-inserted (gaiji)
  (let ((count (lookup-gaiji-get-property gaiji 'count-inserted)))
    (setq count (if count (1+ count) 1))
    (lookup-gaiji-put-property gaiji 'count-inserted count)))

(provide 'lookup-record)

;;; lookup-record.el ends here
