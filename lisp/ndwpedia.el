;;; ndwpedia.el --- Lookup Wikipedia Abst. interface -*- coding: utf-8 -*-

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

;; ndwpedia.el provides the suffix array index searching capability
;; for Wikipedia Abstract files by using `sary' program
;; (http://www.namazu.org/sary/).
;;
;; To use this agent, you must create suffix array index file for
;; XXwiki-latest-abstract.xml files by `mksary -c utf-8'.

;;; Usage:

;;  Specify the directory with XXwiki-latest-abstract.xml files.  
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndwpedia "~/edicts/wikipedia")
;;           ....
;;           ))

;;; Code:

(require 'ndsary)



;;;
;;; Customizable variables
;;;
(defvar ndwpedia-link-notation "《→リンク》")

;;;
;;; Internal Variables
;;;

(defvar ndwpedia-content-tags '("<doc>" . "</doc>"))
(defvar ndwpedia-entry-tags '("<title>Wikipedia: " . "</title>"))
(defvar ndwpedia-replace-entities
  '(
    ("amp" . "&")
    ("lt" . "<")
    ("gt" . ">")
    ("lf" . "\n")))
(defvar ndwpedia-replace-entities-regexp
  (concat "&\\("
          (regexp-opt
           (mapcar 'car ndwpedia-replace-entities))
          "\\);"))

;;;
;;; Interface functions
;;;

(put 'ndwpedia :methods 'ndwpedia-dictionary-methods)
(defun ndwpedia-dictionary-methods (dictionary)
  '(exact prefix suffix substring text))

(put 'ndwpedia :list 'ndwpedia-list)
(defun ndwpedia-list (agent)
  "Return list of dictionaries of Wikipedia Abstract AGENT."
  (let* ((files (directory-files 
                 (expand-file-name (lookup-agent-location agent))
                 nil "\\.xml\\.ary\\'")))
    (mapcar (lambda (name) 
              (lookup-new-dictionary agent (file-name-sans-extension name)))
            files)))

(put 'ndwpedia :title 'ndwpedia-title)
(defun ndwpedia-title (dictionary)
  "Get title of DICTIONARY."
  (or (lookup-dictionary-option dictionary :title)
      (let ((name (lookup-dictionary-name dictionary)))
        (file-name-sans-extension name))))

(put 'ndwpedia :search 'ndwpedia-dictionary-search)
(defun ndwpedia-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let ((string      (lookup-query-string query))
        (method      (lookup-query-method query))
        (file        (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
        (entry-tags  (or (lookup-dictionary-option dictionary :entry-tags t)
                         ndwpedia-entry-tags)))
    (mapcar
     (lambda (x) (lookup-new-entry
                  'regular dictionary (car x) (cdr x)))
     (lookup-with-coding-system 'utf-8
       (ndsary-file-search
        file string method entry-tags nil entry-tags)))))

(put 'ndwpedia :content 'ndwpedia-entry-content)
(defun ndwpedia-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((string     (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (file       (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
         (entry-tags  (or (lookup-dictionary-option dictionary :entry-tags t)
                          ndwpedia-entry-tags)))
    (lookup-with-coding-system 'utf-8
      (ndsary-file-content 
       file (concat (car entry-tags) string (cdr entry-tags))
       (car ndwpedia-content-tags) (cdr ndwpedia-content-tags)))))

(put 'ndwpedia :arranges
     '((reference ndwpedia-arrange-reference)))

;;;
;;; Main Program
;;;

(defun ndwpedia-arrange-reference (entry)
  "Attach contents of ENTRY a link and remove tags."
  (let* ((dictionary (lookup-entry-dictionary entry))
         (entry-tags (or (lookup-dictionary-option dictionary :entry-tags t)
                          ndwpedia-entry-tags)))
    (goto-char (point-min))
    (while (re-search-forward "<url>\\(.+?\\)</url>" nil t)
      (let ((link (match-string 1))
            (start (match-beginning 0)))
        (replace-match ndwpedia-link-notation)
        (lookup-url-set-link start (point) link)))
    (goto-char (point-min))
    (while (re-search-forward
            (concat "<sublink.+?>.*?"
                    "<anchor>\\(.+?\\)</anchor>.*?"
                    "<link>\\(.+?\\)</link></sublink>") nil t)
      (let ((start (match-beginning 0))
            (anchor (match-string 1))
            (link (match-string 2)))
        (replace-match anchor)
        (lookup-url-set-link start (point) link)))
    (goto-char (point-min))
    (while (re-search-forward "</?doc>[\t\n]+" nil t) (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward (car entry-tags) nil t) (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<.+?>" nil t) (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward ndwpedia-replace-entities-regexp nil t)
      (replace-match
       (cdr (assoc (match-string 1) ndwpedia-replace-entities))))))

(provide 'ndwpedia)

;;; ndwpedia.el ends here
