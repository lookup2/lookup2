;;; nducs.el --- Lookup `UCS Name Search' interface -*- coding: utf-8 -*-
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

;; nducs.el provides the simple UCS (unicode) name search interface.

;;; Usage:
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (nducs)
;;           ....
;;           ))

;;; Code:

(require 'lookup)
(require 'descr-text)



;;;
;;; Interface functions
;;;

(put 'nducs :charsets '(ascii))

(put 'nducs :methods '(exact prefix suffix substring regexp))

(put 'nducs :list 'nducs-list)
(defun nducs-list (agent)
  (list (lookup-new-dictionary agent "_")))

(put 'nducs :title 'nducs-title)
(defun nducs-title (dictionary)
  "UCS Name")

(put 'nducs :search 'nducs-dictionary-search)
(defun nducs-dictionary-search (dictionary query)
  (let ((regexp (lookup-query-to-regexp query))
        entries)
    (dolist (ucs-name (ucs-names))
      (if (string-match regexp (car ucs-name))
          (setq entries (cons (lookup-new-entry 
                               'regular dictionary 
                               (char-to-string (cdr ucs-name)) (car ucs-name))
                              entries))))
    (if (> (length entries) lookup-max-hits)
        (message "Too many hits! %s" (length entries))
      entries)))

(put 'nducs :content 'nducs-entry-content)
(defun nducs-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((char (string-to-char (lookup-entry-code entry))))
    (with-temp-buffer
      (insert char "   ")
      (dolist (elt describe-char-unidata-list)
        (let ((val (get-char-code-property char elt))
              description)
          (when val
            (setq description (char-code-property-description elt val))
            (insert (if description
                        (format "  %s: %s (%s)\n" elt val description)
                      (format "  %s: %s\n" elt val))))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'nducs)

;;; nducs.el ends here
