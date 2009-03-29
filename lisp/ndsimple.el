;;; ndsimple.el --- Lookup `simple' interface -*- coding: utf-8 -*-
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

;; ndsimple.el provides the buffered-file index searching capability.
;; The file name must be ended with "*.txt", "*.xml" or "*.sdic".
;;
;; Most of the options are the same as `ndsary' program.  As of it,
;; please refer `ndsary' document for options.

;;; Usage:
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsimple "~/edicts/rfc/" ; where rfc4949.txt exists
;;            :entry-start "   $ " 
;;            :content-start "   $ " :content-end "$"
;;            )
;;           ....
;;           ))

;;; Code:

(require 'lookup)



;;;
;;; Customizable Variables
;;;

(defcustom ndsimple-extension-regexp
  "\\(\\.txt\\)\\|\\(\\.sdic\\)|\\(\\.xml\\)\\'"
  "File Extension to be used as `ndsimple' files.")

;;;
;;; Interface functions
;;;

(put 'ndsimple :methods 'ndsimple-dictionary-methods)
(defun ndsimple-dictionary-methods (dictionary)
  (if (or (and (lookup-dictionary-option dictionary :entry-start t)
               (lookup-dictionary-option dictionary :entry-end t))
          (lookup-dictionary-option dictionary :entry-start-end-pairs t))
      '(exact prefix suffix substring keyword)
    '(exact prefix)))

(put 'ndsimple :list 'ndsimple-list)
(defun ndsimple-list (agent)
  (mapcar (lambda (name) 
            (lookup-new-dictionary agent name))
          (directory-files 
           (expand-file-name (lookup-agent-location agent))
           nil ndsimple-extension-regexp)))

(put 'ndsimple :title 'ndsimple-title)
(defun ndsimple-title (dictionary)
  (or (lookup-dictionary-option dictionary :title)
      (file-name-sans-extension (lookup-dictionary-name dictionary))))

(put 'ndsimple :search 'ndsimple-dictionary-search)
(defun ndsimple-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let ((entry-start (lookup-dictionary-option dictionary :entry-start t))
        (entry-end   (lookup-dictionary-option dictionary :entry-end t))
        (entry-pairs (lookup-dictionary-option dictionary :entry-start-end-pairs t))
        (regular     (lookup-dictionary-option dictionary :regular t))
        (max-hits    (or (lookup-dictionary-option dictionary :max-hits t)
                         lookup-max-hits
                         50))
        (string      (lookup-query-string query))
        (method      (lookup-query-method query))
        (file        (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
        result)
    (setq result
          (if entry-pairs
              (progn
                (if (functionp entry-pairs)
                    (setq entry-pairs (funcall entry-pairs query)))
                (apply 'nconc
                       (mapcar (lambda (x)
                                 (ndsimple-file-entries
                                  file string method (car x) (cdr x) regular))
                               entry-pairs)))
            (ndsimple-file-entries
             file string method entry-start entry-end regular)))
    (message "debug: result=%s" result)
    (if (> (length result) max-hits)
        (list (lookup-new-entry
               'regular dictionary "�"
               (format "Error. More than %s hits. (%d)" max-hits (length result))))
      (mapcar (lambda (x)
                (lookup-new-entry 'regular dictionary (number-to-string (car x)) (cdr x)))
              result))))

(put 'ndsimple :content 'ndsimple-entry-content)
(defun ndsimple-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((code       (string-to-number (lookup-entry-code entry)))
         (dictionary (lookup-entry-dictionary entry))
         (file        (expand-file-name
                       (lookup-dictionary-name dictionary)
                       (lookup-agent-location
                        (lookup-dictionary-agent dictionary))))
         (content-start (lookup-dictionary-option dictionary :content-start t))
         (content-end   (lookup-dictionary-option dictionary :content-end t)))
    (ndsimple-file-content file code content-start content-end)))

;;;
;;; Main Function
;;;

(defun ndsimple-query-regexp (query method start end)
  "Costruct regexp from QUERY string and METHOD.
START and END should surround the entry.
If START is null, beginning of line is assumed.
If END is null, end of line is assumed.
If applied, (match-string 1) should be an entry word."
;; <start>\\(.*search.*\\)</end>
  (let ((query-regexp (regexp-quote query))
        (start-regexp (if start (regexp-quote start) "^"))
        (end-regexp   (if end   (regexp-quote end)   "$")))
    (concat
     (unless (equal method 'keyword) start-regexp)
     "\\("
     (unless (or (equal method 'prefix) (equal method 'exact))
       ".*")
     query-regexp
     (unless (or (equal method 'suffix) (equal method 'exact))
       ".*")
     "\\)"
     (unless (equal method 'keyword) end-regexp))))
     
(defun ndsimple-file-entries (file string method entry-start entry-end regular)
  "Extract entries in FILE with STRING, METHOD, ENTRY-START and ENTRY-END.
REGULAR indicates if search is regular (not used for now).
Extractions are done in accordance with follows."
  (let ((regexp (ndsimple-query-regexp
                 string method entry-start entry-end))
        result buffer)
    (message "regexp=%s" regexp)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq result
              (cons (cons (match-beginning 0) (match-string 1))
                    result)))
      )
    (nreverse result)))

(defun ndsimple-file-content (file point content-start content-end)
  "Get substring of FILE containing POINT, from CONTENT-START to CONTENT-END.
If CONTNET-START is nil, beginning of line is assumed.
If CONTNET-END is nil, end of line is assumed."
  (let (start end buffer result)
    (with-current-buffer (find-file-noselect file)
      (goto-char point)
      (if content-start
          (or (looking-at (regexp-quote content-start))
              (search-backward content-start nil t))
        (goto-char (line-beginning-position)))
      (setq start (point))
      (goto-char point)
      (if content-end
          (search-forward content-end nil t)
        (goto-char (line-end-position)))
      (setq end (point)
            result (buffer-substring-no-properties start end))
      )
    result))
  
(provide 'ndsimple)

;;; ndsimple.el ends here
