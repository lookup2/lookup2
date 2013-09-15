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
;; Supported Dictionary Options
;; (*) … mandatory options
;; |--------------+--------------------------------+-------------------------|
;; | Option Name  | Desc                           | Default                 |
;; |--------------+--------------------------------+-------------------------|
;; | :self        | OpenSearch URL                 | agent location          |
;; | :charsets    | supported character sets       | <:self> or none         |
;; | :title *     | title                          | <:self>                 |
;; | :suggestions | JSON Suggestions URL           | <:self>                 |
;; | :results *   | HTML Results URL               | <:self>                 |
;; | :start-tag   | Text up to this to be removed. | none                    |
;; | :methods     | supported search methods.      | <:suggestions> or exact |
;; | :encoding    | encoding of searched word      | <:self> or utf-8        |
;; | :http-method | "get"  or "post"               | <:self> or "get"        |
;; |--------------+--------------------------------+-------------------------|

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

(defcustom ndweb-w3m "w3m"
  "*Program name of w3m."
  :type 'string
  :group 'ndweb)

(defcustom ndweb-w3m-options '("-halfdump"
                               "-o" "ext_halfdump=1"
                               "-o" "fix_width_conv=1"
                               "-o" "ucs_conv=1"
                               "-O" "UTF-8")
  "*A list of arguments for w3m."
  :type '(repeat (string :tag "option"))
  :group 'ndweb)


;;;
;;; Internal Variables
;;;

;;; internal variables
(defconst ndweb-description-regexp
  "type=[\"']application/opensearchdescription\\+xml[\"'] href=\\(?2:[\"']\\)\\(?1:.+?\\)\\2")

(defconst ndweb-options
  '(:charsets :title :results :start-tag :methods :encoding :http-method :suggestions))


;;;
;;; Internal macros
;;;

(defmacro ndweb-with-url (url &rest body)
  (declare (indent 1))
  `(save-excursion
     (let (buffer)
       (condition-case nil
	   (prog2
	       (setq buffer (url-retrieve-synchronously ,url))
	       (with-current-buffer buffer
		 (set-buffer-multibyte t)
		 (goto-char (point-min))
		 (progn ,@body))
	     (kill-buffer buffer))
	 (error (when (buffer-live-p buffer) (kill-buffer buffer)))))))


;;;
;;; Interface functions
;;;

(put 'ndweb :list 'ndweb-list)
(defun ndweb-list (agent)
  (list (lookup-new-dictionary agent "")))

(put 'ndweb :methods 'ndweb-methods)
(defun ndweb-methods (dict)
  (ndweb-initialize dict)
  (let ((methods (lookup-dictionary-option dict :methods t)))
    (if (and methods (not (functionp methods))) methods
      (if (lookup-dictionary-option dict :suggestions t)
          '(exact prefix)
        '(exact)))))

(put 'ndweb :title 'ndweb-title)
(defun ndweb-title (dict)
  (ndweb-initialize dict)
  (let ((title (lookup-dictionary-option dict :title t)))
    (if (and title (not (functionp title))) title
      (error ":title options is not set! %s" dict))))
    
(put 'ndweb :search 'ndweb-search)
(defun ndweb-search (dict query)
  "Search web DICT for QUERY.
If there is no `suggenstions' URL, then entry with queried  "
  (ndweb-initialize dict)
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
;;; Options Retrieval
;;;

;; Options Dependency
(defun ndweb-initialize (dict)
  "Retrieve DICT options needed but not provided by cache or agent options."
  (let* ((agent    (lookup-dictionary-agent dict))
         (site     (lookup-agent-location agent))
         (agent-options (lookup-agent-options agent))
         (options  (append (lookup-dictionary-options dict)
                           agent-options))
         (title    (plist-get options :title))
         (results  (plist-get options :results))
         (self     (plist-get options :self)))
    (unless (and title results) ;; :title and :results are mandatory options
      (unless self ;; if they are not provided, :self is retrived.
        (setq self (ndweb--opensearch-url site)))
      (if (null self) (error "No proper OpenSearch XML found!"))
      (callf append options (ndweb--opensearch-options self))
      (dolist (option ndweb-options)
        (callf or (lookup-dictionary-option dict option) (plist-get options option))))))

(defun ndweb--opensearch-url (site)
  "Retrieve OpenSearch URL by link.
If it begins with `mycroft:' heading, then mycroft opensearch resource is used."
  (if (string-match "^mycroft:" site)
      (concat "http://mycroft.mozdev.org/externalos.php/" (substring site 8) ".xml")
    (ndweb-with-url (concat "http://" site)
      (when (re-search-forward ndweb-description-regexp nil t)
        (let ((opensearch-url (match-string 1)))
          ;; when it begins with "/", attach parent url to it.
          (if (string-match "^/" opensearch-url)
              (concat "http://" (replace-regexp-in-string "/.*" "" site)
                      opensearch-url)
            opensearch-url))))))

(defun ndweb--opensearch-options (url)
  "Retrive OpenSearch options from OpenSearch URL"
  (let* ((xml (ndweb-with-url url
               (xml-parse-region (point-min) (point-max))))
         (open (assq 'OpenSearchDescription xml))
         (title (caddr (assq 'ShortName open)))
         (encoding (caddr (assq 'InputEncoding open)))
         (encoding (if encoding (intern (downcase encoding))))
         (results ; element
          (cl-find-if (lambda (x)
                        (and (equal (car-safe x) 'Url)
                             (member '(type . "text/html") (cadr x))))
                      open))
         (http-method (assoc-default 'method (cadr results)))
         (http-method (if http-method (downcase http-method)))
         (results-template (assoc-default 'template (cadr results)))
         (suggests
          (cl-find-if (lambda (x)
                        (and (equal (car-safe x) 'Url)
                             (member '(type . "application/x-suggestions+json")
                                     (cadr x))))
                      open))
         (suggests (assoc-default 'template (cadr suggests))))
    ;; Post の場合は、results要素の下位のParam要素のパラメータを繋げる。
    (when (equal http-method "post")
      (callf concat results-template "?")
      (mapc
       (lambda (elem)
         (if (equal (car-safe elem) 'Param)
             (callf concat results-template (assoc-default 'name (cadr elem))
                    "=" (assoc-default 'value (cadr elem)) "&")))
       results))
    (unless title (error "No proper OpenSearch title specified! %s" url))
    (unless results (error "No proper OpenSearch results specified! %s" url))
    (if (and encoding (not (coding-system-p encoding)))
        (error "Emacs do not support encoding %s!" encoding))
    (if (equal http-method "post")
        (error "HTTP Post method is currently not supported!"))
    `(:title ,title :results ,results-template
      ,@(if http-method (list :http-method http-method))
      ,@(if suggests (list :suggestions suggests))
      ,@(if encoding (list :encoding encoding)))))

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

(defun ndweb--get-json (url word)
  (lookup-debug-message "url=%s word=%s" url word)
  (ndweb-with-url
      ;; 前提としてUTF-8
      (replace-regexp-in-string "{searchTerms}" word url)
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
    (let (buffer)
      (unwind-protect
	  (with-current-buffer (setq buffer (url-retrieve-synchronously url))
	    (goto-char (point-min))
	    (let ((header-end (search-forward "\n\n")))
	      (re-search-backward "^Content-Type: \\([-/a-zA-Z]+\\)\\(; charset=\\([-/a-zA-Z]+\\)\\)?" nil t)
	      (goto-char (point-max))
	      (let ((coding-system-for-read 'utf-8))
		(apply 'call-process-region
		       header-end (point-max) ndweb-w3m t t nil
		       (append ndweb-w3m-options
			       `("-T" ,(or (match-string 1) "text/html"))
			       (when (match-beginning 2)
				 `("-I" ,(match-string 3))))))
	      (set-buffer-multibyte t)
	      (buffer-substring header-end (point-max))))
	(when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'ndweb)

;;; ndweb.el ends here
