;;; ndweb.el --- search agent for `Open Search' -*- lexical-binding: t -*-
;; Copyright (C) 2010 Taichi KAWABATA <kawabata.taichi@gmail.com>

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
;; of Open Search.  
;;
;; Example:
;; 
;; (setq lookup-search-agents
;;       '(....
;;         (ndweb "k.hatena.ne.jp")
;;         (ndweb "ja.wikipedia.org")
;;        ....))

;;; Code:

(require 'json)

;;;
;;; Customizable variables
;;;

(defgroup ndweb nil
  "Lookup Open Search interface."
  :group 'lookup-search-agents)

;; If you are interested in specific sites, 
;; you may try 

(defvar ndweb-predefined-agents
  '(("www.google.com"
     "Google (Firefox)"
     "http://suggestqueries.google.com/complete/search?hl=en&client=firefox&hjson=t&&q={searchTerms}"
     "https://www.google.com/#q={searchTerms}&output=search")
    ("en.wikipedia.org"
     "Wikipedia (en)"
     "http://en.wikipedia.org/w/api.php?action=opensearch&search={searchTerms}&namespace=0"
     "http://en.wikipedia.org/w/index.php?search={searchTerms}")
    ("ja.wikipedia.org"
     "Wikipedia (ja)"
     ;;"http://ja.wikipedia.org/w/index.php?title=%E7%89%B9%E5%88%A5:%E6%A4%9C%E7%B4%A2&search={searchTerms}"
     "http://ja.wikipedia.org/w/api.php?action=opensearch&search={searchTerms}&namespace=0"
     "http://ja.wikipedia.org/w/index.php?search={searchTerms}")
    ("k.hatena.ne.jp"
     "はてなダイアリーキーワード検索"
     "http://d.hatena.ne.jp/json/keyword/{searchTerms}"
     "http://d.hatena.ne.jp/keyword?mode=redirect&word={searchTerms}" )))


;;;
;;; Internal Variables
;;;

(defconst ndweb-description-regexp 
  "type=[\"']application/opensearchdescription\\+xml[\"'] href=\\(?2:[\"']\\)\\(?1:.+?\\)\\2")

(defconst ndweb-encoding-regexp "encoding=\\(?2:[\"']\\)\\(?1:.+?\\)\\2")
(defconst ndweb-name-regexp "<ShortName>\\(.+?\\)</ShortName>")
(defconst ndweb-text-regexp ;; assume GET for now
  "type=\"text/html\" method=\"GET\" template=\"\\(.+?\\){searchTerms}\\(.*\\)\"")
(defconst ndweb-suggest-regexp
  "type=\"application/x-suggestions\+json\" template=\"\\(.+?\\){searchTerms}\\(.*\\)\"")

;;;
;;; types
;;;

(put 'ndweb :methods 
     '(exact prefix))
(put 'ndweb :default-method
     'text)


;;;
;;; Interface functions
;;;

(put 'ndweb :list 'ndweb-list)
(defun ndweb-list (agent)
  (list (lookup-new-dictionary agent "")))

(put 'ndweb :title 'ndweb-title)
(defun ndweb-title (dict)
  (let* ((agent    (lookup-dictionary-agent dict))
         (url      (lookup-agent-location agent)))
    (elt (ndweb-parameters url) 0)))
    
(put 'ndweb :search 'ndweb-search)
(defun ndweb-search (dict query)
  "Search web DICT for QUERY."
  (let* ((agent    (lookup-dictionary-agent dict))
         (domain   (lookup-agent-location agent))
         (suggest  (elt (ndweb-parameters domain) 1))
         (string   (lookup-query-string  query))
         (json     (ndweb-get-json suggest string))
         (terms    (elt json 1)))
    (mapcar (lambda (term)
              (lookup-new-entry 'regular dict term))
            terms)))

(put 'ndweb :content 'ndweb-content)
(defun ndweb-content (entry)
  (let* ((dict     (lookup-entry-dictionary entry))
         (agent    (lookup-dictionary-agent dict))
         (domain   (lookup-agent-location agent))
         (html     (elt (ndweb-parameters domain) 2))
         (word     (lookup-entry-code entry)))
    (replace-regexp-in-string "{searchTerms}" (url-encode-url word) html)))

(put 'ndweb :arranges '((reference lookup-arrange-references-url)))

;;;
;;; Internal functions
;;;

;(defmacro ndweb-with-url (url &rest body)
;  (declare (indent 1))
;  `(let* ((buffer 
;           (url-retrieve-synchronously ,url)))
;     (when buffer
;       (switch-to-buffer buffer)
;       (set-buffer-multibyte t)
;       (prog1 (progn ,@body)
;         (kill-buffer buffer)))))
;
;(defun ndweb-url-contents (url)
;  (ndweb-with-url url
;    (buffer-string)))
;
;(defun ndweb-opensearch-url (url)
;  (ndweb-with-url url
;    (goto-char (point-min))
;    (when (re-search-forward ndweb-description-regexp nil t)
;      (let ((search-url (match-string 1)))
;        ;; when it begins with "/", attach parent url to it.
;        (if (string-match "^/" search-url)
;            (concat (replace-regexp-in-string "\\(://[^/]+/\\).*" "\\1" url)
;                    search-url)
;          search-url)))))
;
;(defun ndweb-opensearch-xml (url)
;  (ndweb-with-url url
;    (xml-parse-region (point-min) (point-max))))
;
;(defun ndweb-parse-xml (xml)
;  "XMLデータから、encoding情報等を取り出す。"
;  (let (open name ienc html sugg)
;    (setq open (assq 'OpenSearchDescription xml)
;          name (caddr (assq 'ShortName open))
;          ienc (caddr (assq 'InputEncoding open))
;          html (find-if (lambda (x)
;                          (and (equal (car-safe x) 'Url)
;                               (member '(type . "text/html") (cadr x))))
;                        open)
;          sugg (find-if (lambda (x)
;                          (and (equal (car-safe x) 'Url)
;                               (member '(type . "application/x-suggestions+json")
;                                       (cadr x))))
;                        open))
;    (list name ienc (assoc-default 'template (cadr html))
;                    (assoc-default 'template (cadr sugg)))))

(defun ndweb-get-json (url word)
  (let* ((buffer (url-retrieve-synchronously 
                  (replace-regexp-in-string "{searchTerms}"
                                            (url-encode-url word) url))))
    (prog1
        (with-current-buffer buffer
          (set-buffer-multibyte t)
          (goto-char (point-min))
          (search-forward "\n\n")
          (json-read))
      (kill-buffer buffer))))

(defun ndweb-parameters (domain)
  (assoc-default domain ndweb-predefined-agents))

(provide 'ndweb)

;;; ndweb.el ends here
