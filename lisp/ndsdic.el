;;; ndsdic.el --- Lookup sdic dictionary interface -*- coding: utf-8 -*-

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

;; ndsdic.el provides the suffix array index searching capability
;; for SDIC dictionary by using `sary' program
;; (http://www.namazu.org/sary/).
;;
;; To use this agent, you must create suffix array index file for
;; *.sdic files by `mksary -c euc-jp'.

;;; Usage:

;;  Specify the directory with XXX.xml.ary files.  
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsdic "~/edicts/sdic/")
;;           ....
;;           ))

;;; Code:

(require 'ndsary)



;;;
;;; Internal Variables
;;;

(defvar ndsdic-replace-entities
  '(
    ("amp" . "&")
    ("lt" . "<")
    ("gt" . ">")
    ("lf" . "\n")))

(defvar support-sdic-replace-entities-regexp
  (concat "&\\("
          (regexp-opt
           (mapcar 'car support-sdic-replace-entities))
          "\\);"))

(defvar ndsdic-entry-tags '("<K>" . "</K>"))

;;;
;;; Interface functions
;;;

(put 'ndsdic :charsets '(ascii japanese-jisx0208))

(put 'ndsdic :methods 'ndsdic-dictionary-methods)
(defun ndsdic-dictionary-methods (dictionary)
  '(exact prefix suffix substring text))

(put 'ndsdic :list 'ndsdic-list)
(defun ndsdic-list (agent)
  "Return list of dictionaries of btonic AGENT."
  (let* ((files (directory-files 
                 (expand-file-name (lookup-agent-location agent))
                 nil "\\.sdic\\.ary\\'")))
    (mapcar (lambda (name) 
              (lookup-new-dictionary agent (file-name-sans-extension name)))
            files)))

(put 'ndsdic :title 'ndsdic-title)
(defun ndsdic-title (dictionary)
  "Get title of DICTIONARY."
  (or (lookup-dictionary-option dictionary :title)
      (let ((name (lookup-dictionary-name dictionary)))
        (file-name-sans-extension name))))

(put 'ndsdic :search 'ndsdic-dictionary-search)
(defun ndsdic-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let ((string      (lookup-query-string query))
        (method      (lookup-query-method query))
        (regular     nil)
        (file        (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
        (coding      (or (lookup-dictionary-option dictionary :coding)
                         'euc-jp)))
    (mapcar
     (lambda (x) (lookup-new-entry
                  'regular dictionary (car x) (cdr x)))
     (lookup-with-coding-system coding
       (ndsary-file-search
        file string method ndsdic-entry-tags nil
        ndsdic-entry-tags)))))

(put 'ndsdic :content 'ndsdic-entry-content)
(defun ndsdic-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((string     (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (file       (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
         (coding     (or (lookup-dictionary-option dictionary :coding)
                         'euc-jp)))
    (lookup-with-coding-system coding
      (ndsary-file-content 
       file (concat (car ndsdic-entry-tags) string (cdr ndsdic-entry-tags))))))

(put 'ndsdic :arrange-table
     '((replace   ndsdic-arrange-replace)))

;;;
;;; Main Program
;;;

(defun ndsdic-arrange-replace (entry)
  "Arrange content of ENTRY."
  (goto-char (point-min))
  (if (looking-at "\n+") (replace-match ""))
  (while (re-search-forward support-sdic-replace-entities-regexp nil t)
    (replace-match
     (cdr (assoc (match-string 1) support-sdic-replace-entities))))
  (goto-char (point-min))
  (while (re-search-forward "^.*<H>\\(.+\\)</H>.*<K>.+</K>\\(.*\\)" nil t)
    (replace-match "\\1\n\\2\n" t))
  (goto-char (point-min))
  (while (re-search-forward "^.*<K>\\(.+\\)</K>\\(.*\\)" nil t)
    (replace-match "\\1\n\\2\n" t)))

(provide 'ndsdic)

;;; ndsdic.el ends here
