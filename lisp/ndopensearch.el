;;; ndopensearch.el --- search agent for `Open Search'
;; Copyright (C) 2009 Taichi KAWABATA <kawabata.taichi@gmail.com>

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

;; This agent provides capability to search using `suggestion' feature
;; of Open Search.  You need to install `w3m' package to use this
;; agent.
;;
;; Currently, only "Hatena Keyword" search is supported.
;; Request for any other search is welcome.
;;
;; Example:
;; 
;; (setq lookup-search-agents
;;       '(....
;;         (ndopensearch "http://k.hatena.ne.jp")   ;; Search for Hatena Keywords.
;;        ))

;;; Code:

(require 'json)
(require 'w3m nil t)
(require 'ndmisc)
(require 'lookup-content)

;;;
;;; Customizable variables
;;;

(defgroup ndopensearch nil
  "Lookup Open Search interface."
  :group 'lookup-search-agents)

(defcustom ndopensearch-w3m-program "w3m"
  "*Command name and location of `w3m'."
  :type 'string
  :group 'ndopensearch)

;;(defcustom ndopensearch-w3m-program-options '("-d")
;;  "*Options for searching the query for `w3m'."
;;  :type '(repeat :tag "Options" string)
;;  :group 'ndopensearch)


;;;
;;; Internal Variables
;;;

(defvar ndopensearch-link-map nil)
(unless ndopensearch-link-map
  (setq ndopensearch-link-map (copy-keymap lookup-content-mode-map))
  (define-key ndopensearch-link-map "\C-m" 'ndopensearch-follow-link)
  (define-key ndopensearch-link-map [mouse-2] 'ndopensearch-mouse-follow))

(defvar ndopensearch-description-regexp "type=[\"']application/opensearchdescription\\+xml[\"'] href=\\(?2:[\"']\\)\\(?1:.+?\\)\\2")

(defvar ndopensearch-encoding-regexp "encoding=\\(?2:[\"']\\)\\(?1:.+?\\)\\2")

(defvar ndopensearch-name-regexp "<ShortName>\\(.+?\\)</ShortName>")

(defvar ndopensearch-text-regexp ;; assume GET for now
  "type=\"text/html\" method=\"GET\" template=\"\\(.+?\\){searchTerms}\\(.*\\)\"")

(defvar ndopensearch-suggest-regexp
  "type=\"application/x-suggestions\+json\" template=\"\\(.+?\\){searchTerms}\\(.*\\)\"")

;;;
;;; types
;;;

(put 'ndopensearch :methods 
     '(exact prefix text))
(put 'ndopensearch :arranges 
     '((structure ndopensearch-arrange-structure)))
(put 'ndopensearch ':default-method
     'text)


;;;
;;; Interface functions
;;;

(put 'ndopensearch :list #'ndopensearch-list)
(defun ndopensearch-list (agent)
  (if (and (executable-find ndopensearch-w3m-program)
           (functionp #'w3m))
      (let* ((url (lookup-agent-location agent))
             (xml 
              (with-temp-buffer
                (w3m-retrieve (concat "about://source/" url))
                (goto-char (point-min))
                (when (re-search-forward ndopensearch-description-regexp nil t)
                  (let ((osdd-url (match-string 1)))
                    (delete-region (point-min) (point-max))
                    (w3m-retrieve (concat "about://source/" osdd-url))))
                (goto-char (point-min))
                ;;(when (re-search-forward ndopensearch-encoding-regexp nil t)
                ;;  (setq encoding (match-string 1))) ;; assume utf-8 for now.
                (set-buffer-multibyte t)
                (decode-coding-region (point-min) (point-max) 'utf-8)
                (xml-parse-region (point-min) (point-max))))
             (open (assq 'OpenSearchDescription xml))
             (name (caddr (assq 'ShortName open)))
             (ienc (caddr (assq 'InputEncoding open)))
             (html (find-if (lambda (x)
                              (and (consp x) 
                                   (equal (car x) 'Url)
                                   (member '(type . "text/html") (cadr x))))
                            open))
             (sugg (find-if (lambda (x)
                              (and (consp x)
                                   (equal (car x) 'Url)
                                   (member '(type . "application/x-suggestions+json")
                                           (cadr x))))
                            open)))
        (if (and name ienc html sugg)
            (let* ((dict (lookup-new-dictionary agent ""))
                   (dict-id (lookup-dictionary-id dict)))
              (setq html (cdr (assoc 'template (cadr html))))
              (setq sugg (cdr (assoc 'template (cadr sugg))))
              (lookup-put-property dict-id :title name)
              (lookup-put-property dict-id :encoding (intern (downcase ienc)))
              (lookup-put-property dict-id :html html)
              (lookup-put-property dict-id :suggest sugg)
              (list dict))
          (message "ndopensearch: error. osdd file not supported format.")
          nil))
    (message "ndopensearch: error.  `w3m' not found.")
    nil))

(put 'ndopensearch :title #'ndopensearch-title)
(defun ndopensearch-title (dict)
  (lookup-get-property (lookup-dictionary-id dict) :title))

(put 'ndopensearch :search #'ndopensearch-search)
(defun ndopensearch-search (dict query)
  "Search opensearch DICT for QUERY."
  (let* ((dict-id   (lookup-dictionary-id dict))
         (sugget    (lookup-get-property dict-id :suggest))
         (encoding  (lookup-get-property dict-id :encoding))
         (suggest   (lookup-get-property dict-id :suggest))
         (query-method  (lookup-query-method  query))
         (query-string  (lookup-query-string  query))
         (query-pattern (lookup-query-pattern query))
         (suggest   (replace-regexp-in-string 
                     "{searchTerms}" 
                     (ndmisc-encode-xwfu query-string  encoding) suggest))
         (terms))
    (with-temp-buffer
      (w3m-retrieve (concat "about://source/" suggest))
      (set-buffer-multibyte t) ;; assume utf-8
      (goto-char (point-min))
      (setq terms (elt (json-read) 1))
      (mapcar (lambda (x) 
                (lookup-new-entry 'regular dict x))
              terms))))

(put 'ndopensearch :content #'ndopensearch-content)
(defun ndopensearch-content (entry)
  (let* ((dict     (lookup-entry-dictionary entry))
         (dict-id  (lookup-dictionary-id dict))
         (word     (lookup-entry-code entry))
         (encoding (lookup-get-property dict-id :encoding))
         (html     (lookup-get-property dict-id :html))
         (url      (replace-regexp-in-string 
                    "{searchTerms}" 
                    (ndmisc-encode-xwfu word encoding) html)))
    (with-temp-buffer
      (w3m-retrieve url)
      (set-buffer-multibyte 'to)
      (buffer-string))))

(defun ndopensearch-arrange-structure (entry)
  (w3m-region (point-min) (point-max))
  (remove-text-properties 
   (point-min) (point-max) 
   '(balloon-help face help-echo mouse-face 
     keymap help-echo w3m-anchor-sequence w3m-balloon-help
     w3m-safe-url-regexp)))

(defun ndopensearch-set-link (start end file)
  (add-text-properties start end
                       (list
                        'keymap ndopensearch-link-map
                        'face 'lookup-reference-face
                        'mouse-face 'highlight
                        'lookup-tab-stop t
                        'ndopensearch-link file)))

(defun ndopensearch-get-link (&optional pos)
  (get-text-property (or pos (point)) 'ndopensearch-link))

(defun ndopensearch-follow-link ()
  (interactive)
  (let ((url (ndopensearch-get-link (point))))
    (browse-url url)))

(defun ndopensearch-mouse-follow (event)
  "Play the binary you click on."
  (interactive "e")
  (mouse-set-point event)
  (ndopensearch-follow-link))

(provide 'ndopensearch)

;;; ndopensearch.el ends here
