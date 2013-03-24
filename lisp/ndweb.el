;;; ndweb.el --- search agent for `Open Search' -*- lexical-binding: t -*-
;; Copyright (C) 2010,2013 Taichi KAWABATA <kawabata.taichi@gmail.com>

;; Author: Taichi KAWABATA <kawabata.taichi@gmail.com>
;; Keywords: Open Search

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

;; This agent provides web-based search engine support based on
;; OpenSearch API.  If agent location has a prefix of `mycroft:', then
;; Microft OpenSearch interaface of
;; http://mycroft.mozdev.org/externalos.php/" + name + ".xml" will be
;; used.

;; Example:
;; 
;; (setq lookup-search-agents
;;       '(....
;;         (ndweb "k.hatena.ne.jp")
;;         (ndweb "ja.wikipedia.org")
;;         (ndweb "mycroft:XXX")
;;         (ndweb "www.example.com"
;;                :self "http://www.example.com/opensearch.xml")
;;        ....))
;;
;; Currently, POST-method Search Engine is not supported.
;;
;; Supported Dictionary Options (You can not specify them in agent option!)
;;
;; |--------------+-------------------------------------|
;; | :self        | OpenSearch URL                      |
;; | :charsets    | supported character sets            |
;; | :title       | title                               |
;; | :suggestions | JSON Suggestions URL                |
;; | :results     | HTML Results URL                    |
;; | :start-tag   | Text Up to this tag to be removed.  |
;; | :methods     | supported search methods.           |
;; | :encoding    | encoding of searched word           |
;; | :http-method | "get" or "post" (not supported yet) |
;; |--------------+-------------------------------------|

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'json)
(require 'lookup)

;;;
;;; Customizable variables
;;;

(defgroup ndweb nil
  "Lookup Web-Search interface."
  :group 'lookup-search-agents)


;;;
;;; Customizable Variables
;;;

(defgroup ndweb nil
  "Lookup ndweb interface."
  :group 'lookup-agents)

(defcustom ndweb-w3m "w3m"
  "*Program name of w3m."
  :type 'string
  :group 'ndweb)

(defcustom ndweb-w3m-options '("-T" "text/html"
                               "-halfdump"
                               "-o" "ext_halfdump=1"
                               "-o" "fix_width_conv=1"
                               "-o" "ucs_conv=1"
                               "-O" "UTF-8")
  "*A list of arguments for w3m."
  :type '(repeat (string :tag "option"))
  :group 'ndweb)


;;;
;;; Interface functions
;;;

(put 'ndweb :list 'ndweb-list)
(defun ndweb-list (agent)
  (list (lookup-new-dictionary agent "")))

(put 'ndweb :methods 'ndweb-methods)
(defun ndweb-methods (dict)
  (let ((methods (lookup-dictionary-option dict :methods t)))
    (if (and methods (not (functionp methods))) methods
      (if (lookup-dictionary-option dict :suggestions t)
          '(exact prefix)
        '(exact)))))

(put 'ndweb :title 'ndweb-title)
(defun ndweb-title (dict)
  (let ((title (lookup-dictionary-option dict :title t)))
    (if (and title (not (functionp title))) title
      (error ":title options is not set! %s" dict))))
    
(put 'ndweb :search 'ndweb-search)
(defun ndweb-search (dict query)
  "Search web DICT for QUERY.
If there is no `suggenstions' URL, then entry with queried  "
  (let* ((suggest (lookup-dictionary-option dict :suggestions t))
         (method  (lookup-query-method query))
         (string  (lookup-query-string query)))
    (mapcar
     (lambda (code)
       (if (listp code)
           (lookup-new-entry 'regular dict (car code) (elt code 1))
         (lookup-new-entry 'regular dict code)))
     (pcase suggest
       ((pred null) (list string))
       ((pred functionp) (funcall suggest
                                  (lookup-query-string query)
                                  (lookup-query-method query)
                                  (lookup-query-pattern query)))
       (t (let* ((json  (if suggest (ndweb--get-json suggest string)))
                 (terms (if json (elt json 1))))
            (if (and (not (eq [] terms)) (equal method 'exact))
                (list (aref terms 0)) terms)))))))

(put 'ndweb :content 'ndweb-content)
(defun ndweb-content (entry)
  (let ((dict (lookup-entry-dictionary entry)))
    (let* ((results (lookup-dictionary-option dict :results t))
           (results (if (functionp results) (funcall results (lookup-entry-code entry)
                                                     (lookup-entry-heading entry)) results))
           (encoding (lookup-dictionary-option dict :encoding t))
           (method   (lookup-dictionary-option dict :http-method t))
           (word    (lookup-entry-code entry))
           (url     (replace-regexp-in-string
                     "{searchTerms}"
                     (url-encode-url
                      (if encoding (encode-coding-string word encoding) word))
                     results)))
      (concat 
       (ndweb--url-contents url method)
       "<a href=\"" url "\">【Original Site】</a>"))))

(put 'ndweb :arrange-table '((replace ndweb-remove-to-start)
                             (reference ndweb-arrange-references
                                        lookup-arrange-references-url
                                        ndweb-arrange-tags)
                             (fill      lookup-arrange-nofill)))

;;;
;;; Arrange functions
;;;

(defun ndweb-remove-to-start (entry)
  (let ((start-tag (lookup-dictionary-option
                        (lookup-entry-dictionary entry) :start-tag t)))
    (when (and start-tag (re-search-forward start-tag nil t))
      (delete-region (point-min) (match-beginning 0)))))

(defun ndweb-arrange-references (entry)
  (let ((dictionary (lookup-entry-dictionary entry))
        (case-fold-search t))
    (while (re-search-forward "<a .*?href=\"\\(https?://.+?\\)\".*?>\\(.+?\\)</a>" nil t)
      (let* ((href (match-string-no-properties 1)))
        (lookup-set-link (match-beginning 2) (match-end 2)
                         (lookup-new-entry 'url dictionary href href))
        (delete-region (match-end 2) (match-end 0))
        (delete-region (match-beginning 0) (match-beginning 2))))
    (goto-char (point-min))))

(defun ndweb-arrange-tags (_entry)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward "<b>\\(.+?\\)</b>" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(face lookup-bold-face))
      (delete-region (match-end 1) (match-end 0))
      (delete-region (match-beginning 0) (match-beginning 1)))
    (goto-char (point-min))
    (while (re-search-forward "<i>\\(.+?\\)</i>" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(face lookup-italic-face))
      (delete-region (match-end 1) (match-end 0))
      (delete-region (match-beginning 0) (match-beginning 1)))
    (goto-char (point-min))
    (while (re-search-forward "<_SYMBOL.*?>\\(.+?\\)</_SYMBOL>" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(face lookup-emphasis-face))
      (delete-region (match-end 1) (match-end 0))
      (delete-region (match-beginning 0) (match-beginning 1)))
    (goto-char (point-min))
    (while (re-search-forward "</?span.*?>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<.+?>" nil t)
      (replace-match ""))))

;;;
;;; Internal functions
;;;

(defmacro ndweb-with-url (url &rest body)
  (declare (indent 1))
  `(save-excursion
     (let* (buffer)
       (condition-case nil
           (prog2
             (setq buffer (url-retrieve-synchronously ,url))
             (with-current-buffer buffer
               (set-buffer-multibyte t)
               (goto-char (point-min))
               (progn ,@body))
             (kill-buffer buffer))
         (error (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun ndweb--get-json (url word)
  (lookup-debug-message "url=%s word=%s" url word)
  (ndweb-with-url 
      ;; 前提としてUTF-8
      (replace-regexp-in-string "{searchTerms}" word url)
    (set-buffer-multibyte t)
    (search-forward "\n\n")
    (json-read)))

(defun ndweb--url-contents (url &optional method)
  (let (url-request-method url-request-extra-headers url-request-data)
    (if (equal method "post")
        (setq url-request-method "POST"
              url-request-extra-headers 
              '(("Content-Type" . "application/x-www-form-urlencoded"))
              url-request-data
              (replace-regexp-in-string "^.+\\?" "" url)))
    (ndweb-with-url url
      (search-forward "\n\n")
      (delete-region (point-min) (point))
      (apply 'call-process-region (point-min) (point-max) ndweb-w3m t
             (current-buffer) nil ndweb-w3m-options)
      (buffer-string))))

(provide 'ndweb)

;;; ndweb.el ends here
