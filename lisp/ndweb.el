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

;; Example:
;; 
;; (setq lookup-search-agents
;;       '(....
;;         (ndweb "k.hatena.ne.jp")
;;         (ndweb "ja.wikipedia.org")
;;         (ndweb "mycroft:XXX")
;;        ....))
;; http://mycroft.mozdev.org/externalos.php/" + name + ".xml"
;;
;; Currently, non-UTF-8 search engine and POST-method Search Engine
;; is not supported.

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'json)
(require 'lookup)

;;;
;;; Customizable variables
;;;

(defgroup ndweb nil
  "Lookup Open Search interface."
  :group 'lookup-search-agents)

;; Providing `:self' option would greatly speed up the set-up speed.
;; Providing `:results' would even make the search speed faster.
(defvar ndweb-sites
  '(;; English
    (:charsets (ascii)
     :default-suggestions
     "http://en.wiktionary.org/w/api.php?action=opensearch&search={searchTerms}&namespace=0"
     :sites
     (
      ("en.wikipedia.org"
       :self "http://en.wikipedia.org/w/opensearch_desc.php")
      ("en.wiktionary.org")
      ("www.google.com" :title "Google (Firefox)"
       :suggestions "http://suggestqueries.google.com/complete/search?hl=en&client=firefox&hjson=t&&q={searchTerms}"
       :results "https://www.google.com/#q={searchTerms}&output=search")
      ("www.ldoceonline.com"
       :self "http://www.ldoceonline.com/widgets/ldoce_opensearch.xml")
      ("dictionary.cambridge.org"
       :self "http://dictionary.cambridge.org/gadgets/british/opensearch.xml")
      ;("mycroft:webster") ; encoding is ISO-8859-1
      ;("mycroft:webster-med") ; encoding is ISO-8859-1
      ;("mycroft:webster-thsrs") ; encoding is ISO-8859-1
      ("mycroft:oald8" :start-tag "<_id id=\"main-container\">")
      ("mycroft:yahoo_dictionary")
      ("www.onelook.com" :self "http://www.onelook.com/osdf.xml")
      ("www.oxfordjournals.org" :self "http://www.oxfordjournals.org/resource/xml/opensearch.xml")
      ("mycroft:blaut" :title "Brigish Library Author Search")
      ("mycroft:bltit" :title "Brigish Library Title Search")
      ("mycroft:collins-cobuild")
      ;; English-to-Japanese
      ("mycroft:kenkwaeichujiten-cl" :title "研究社新和英中辞典 (Excite)")
      ("mycroft:yahoodicenjp")
      ("mycroft:eijiro")
      ("mycroft:eijiro-collocation" :title "英辞郎 頻度集計")
      ("mycroft:eijiro-kwic" :title "英辞郎 整列表示")
      ))
    (;; Japanese
     :charsets (ascii japanese-jisx0213-1 japanese-jisx0213-2)
     :default-suggestions
     "http://ja.wiktionary.org/w/api.php?action=opensearch&search={searchTerms}&namespace=0"
     :sites
     (("ja.wikipedia.org" :self "http://ja.wikipedia.org/w/opensearch_desc.php")
      ("ja.wiktionary.org")
      ("k.hatena.ne.jp" :self "http://d.hatena.ne.jp/opensearch/keyword.xml"
       :start-tag "<b><a name=\"keywordbody\"")
      ("mycroft:daijirin" :title "大辞林 (Yahoo)" :start-tag "<_id id=\"main-d\">")
      ("mycroft:daihyakkazensho-cl" :title "小学館日本大百科全書 (Yahoo)")
      ("mycroft:sanseidodaijirin-cl" :title "三省堂 大辞林 (Excite)")
      ("mycroft:yahoodicjp_thesaurus")
      ;; Japanese-to-English
      ("mycroft:kenkeiwachujiten-cl" :title "研究社新英和中辞典 (Excite)")
      ("www.excite.co.jp/dictionary"
       :self "http://www.excite.co.jp/search/opensearch/xml/dictionary/english/")
      ("mycroft:yahoodicjpen")
      ;; Japanese-to-Chinese
      ("mycroft:bitex" :title "BitEx 日中辞書")
      ("mycroft:hjenglish-j2c" :title "沪江小D中日词典")))
    (;; Chinese
     :charsets (ascii chinese-gb2312)
     :default-suggestions
     "http://zh.wiktionary.org/w/api.php?action=opensearch&search={searchTerms}&namespace=0"
     :sites
     (("zh.wikipedia.org")
      ("zh.wiktionary.org")
      ("www.thefreedictionary.com"
       :self "http://www.thefreedictionary.com/_/open-search.xml")
      ("www.iciba.com"
       :self "http://res.iciba.com/dict/opensearch_desc.xml")
      ;; Chinese-to-Japanese
      ("bitex-cn.com" :title "BitEx 中日辞書"
       :results "http://www.bitex-cn.com/search_result.php?deal_type=jp2cn&amp;keywords={searchTerms}")
      ; ("mycroft:frelax-zhpy_jp" :title "書虫 Pinyin") ; POST
      ("mycroft:hjenglish-c2j" :title "沪江小D日中词典")))))
      


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

(defconst ndweb-w3m         "w3m")
(defconst ndweb-w3m-options '("-halfdump"
                              "-o" "ext_halfdump=1"
                              "-o" "fix_width_conv=1"
                              "-o" "ucs_conv=1"
                              "-O" "UTF-8"))


;;;
;;; Interface functions
;;;

(put 'ndweb :methods '(exact prefix))
(put 'ndweb :default-method 'exact)

(put 'ndweb :list 'ndweb-list)
(defun ndweb-list (agent)
  (list (lookup-new-dictionary agent "")))

(put 'ndweb :title 'ndweb-title)
(defun ndweb-title (dict)
  (ndweb-initialize-dictionary dict)
  (lookup-dictionary-option dict :title))
    
(put 'ndweb :search 'ndweb-search)
(defun ndweb-search (dict query)
  "Search web DICT for QUERY."
  (ndweb-initialize-dictionary dict)
  (let* ((suggest  (or (lookup-dictionary-option dict :suggestions)
                       (lookup-dictionary-option dict :default-suggestions)))
         (method   (lookup-query-method query))
         (string   (lookup-query-string query))
         (json     (ndweb-get-json suggest string))
         (terms    (elt json 1)))
    (when (and (not (eq [] terms)) (equal method 'exact))
      (setq terms (list (aref terms 0))))
    (mapcar (lambda (term)
              (lookup-new-entry 'regular dict term))
            terms)))

(put 'ndweb :content 'ndweb-content)
(defun ndweb-content (entry)
  (let ((dict (lookup-entry-dictionary entry)))
    (ndweb-initialize-dictionary dict)
    (let* ((results (lookup-dictionary-option dict :results))
           (word    (lookup-entry-code entry))
           (url     (replace-regexp-in-string
                     "{searchTerms}" (url-encode-url word) results)))
      (concat 
       (ndweb-url-contents url)
       "<a href=\"" url "\">【Original Site】</a>"))))

(put 'ndweb :arrange-table '((replace ndweb-remove-to-start)
                             (reference ndweb-arrange-references
                                        lookup-arrange-references-url
                                        ndweb-arrange-tags)
                             (fill      lookup-arrange-nofill)))

(defun ndweb-initialize-dictionary (dict)
  (unless (lookup-dictionary-option dict :results)
    (let* ((agent (lookup-dictionary-agent dict))
           (site (lookup-agent-location agent))
           (options (ndweb-site-options site)))
      (lookup-debug-message "options=%s" options)
      (loop for opt-name in
            '(:title :self :suggestions :results :charsets
                     :default-suggestions :start-tag)
            do (callf unless (lookup-dictionary-option dict opt-name)
                 (plist-get options opt-name))))))

;;;
;;; Arrange functions
;;;

(defun ndweb-remove-to-start (entry)
  (let ((start-tag (lookup-dictionary-option
                        (lookup-entry-dictionary entry) :start-tag)))
    (when start-tag
      (when (re-search-forward start-tag nil t)
        (delete-region (point-min) (match-beginning 0))))))

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

;; When error occured during contents retrieval, signal error.
;; Otherwise, it will not signal error.
(defmacro ndweb-with-url (url &rest body)
  (declare (indent 1))
  `(save-excursion
     (let* ((buffer (url-retrieve-synchronously ,url)))
       (condition-case nil
           (prog1
               (with-current-buffer buffer
                 (set-buffer-multibyte t)
                 (progn ,@body))
             (kill-buffer buffer))
         (kill-buffer buffer)))))

(defun ndweb-site-options (site)
  "Return required options (:title, :suggestions, :results) of SITE."
  (let* ((options  (ndweb-site-predefined-options site))
         (title    (plist-get options :title))
         (results  (plist-get options :results))
         (self     (plist-get options :self)))
    (unless (and title results)
      (unless self
        (setq self (ndweb-opensearch-url site)))
      (lookup-debug-message "self=%s" self)
      (if (null self) (error "No proper OpenSearch XML found!"))
      (callf append options (ndweb-opensearch-options self)))
    options))

(defun ndweb-site-predefined-options (site)
  (cdr (cl-mapcan
        (lambda (set)
          (let* ((sites (plist-get set :sites))
                 (site-opts (cl-assoc site sites :test 'equal)))
            (when site-opts
              (append site-opts (butlast set 2)))))
        ndweb-sites)))

(defun ndweb-opensearch-url (site)
  (if (string-match "^mycroft:" site)
      (concat "http://mycroft.mozdev.org/externalos.php/" (substring site 8) ".xml")
    (ndweb-with-url (concat "http://" site)
      (goto-char (point-min))
      (when (re-search-forward ndweb-description-regexp nil t)
        (let ((opensearch-url (match-string 1)))
          ;; when it begins with "/", attach parent url to it.
          (if (string-match "^/" opensearch-url)
              (concat "http://" (replace-regexp-in-string "/.*" "" site)
                      opensearch-url)
            opensearch-url))))))

(defun ndweb-opensearch-options (url)
  "Retrive OpenSearch options from OpenSearch URL"
  (let* ((xml (ndweb-with-url url
               (xml-parse-region (point-min) (point-max))))
         (open (assq 'OpenSearchDescription xml))
         (title (caddr (assq 'ShortName open)))
         (ienc (caddr (assq 'InputEncoding open)))
         (results ; element
          (cl-find-if (lambda (x)
                        (and (equal (car-safe x) 'Url)
                             (member '(type . "text/html") (cadr x))))
                      open))
         ;; HTTP GET  => template-string
         ;; HTTP POST => (template . ((param . val) (param . val) ...))
         (results 
          (if (equal (assoc-default 'method (cadr results)) "post")
              (cons (assoc-default 'template (cadr results))
                    (cl-mapcan
                     (lambda (elem) 
                       (if (equal (car-safe elem) 'Param)
                           (list (cons (assoc-default 'name (cadr elem))
                                       (assoc-default 'value (cadr elem))))))
                     results))
            (assoc-default 'template (cadr results))))
         (suggests
          (cl-find-if (lambda (x)
                        (and (equal (car-safe x) 'Url)
                             (member '(type . "application/x-suggestions+json")
                                     (cadr x))))
                      open))
         (suggests (assoc-default 'template (cadr suggests))))
    (message "results=%s" results)
    (if (listp results)
        (error "HTTP Post method is currently not supported!"))
    (if (listp results)
        (error "HTTP Post method is currently not supported!"))
    (if (and ienc (not (equal (downcase ienc) "utf-8")))
        (error "Non UTF-8 encoding not supported!"))
    `(:title ,title :results ,results
      ,@(if suggests (list :suggestions suggests)))))

(defun ndweb-get-site-options (site)
  (cl-mapcan
   (lambda (set)
     (let* ((sites (plist-get set :sites))
            (site-opts (cl-assoc site sites :test 'equal)))
       (when site-opts
         (append site-opts (butlast set 2)))))
   ndweb-sites))

(defun ndweb-get-json (url word)
  (lookup-debug-message "url=%s word=%s" url word)
  (ndweb-with-url 
      ;(replace-regexp-in-string "{searchTerms}" (url-encode-url word) url)
      (replace-regexp-in-string "{searchTerms}" word url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (search-forward "\n\n")
    (json-read)))

(defun ndweb-parameters (domain)
  (assoc-default domain ndweb-predefined-agents))

(defun ndweb-url-contents (url)
  (let* ((args (append ndweb-w3m-options (list url))))
    (with-temp-buffer
      (lookup-debug-message "w3m args=%s" args)
      (apply 'call-process ndweb-w3m nil (current-buffer) nil args)
      (buffer-string))))

(provide 'ndweb)

;;; ndweb.el ends here
