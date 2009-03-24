;;; ndmisc.el --- miscellaneous pseudo-agents -*- coding: utf-8 -*-
;; Copyright (C) 2009 Lookup Development Team

;; Author: KAWABATA, Taichi (kawabata.taichi@gmail.com)
;; Version: $Id: ndmisc.el,v 1.5 2009/03/24 23:00:56 kawabata Exp $

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

;; This agent will let user to search the various misc. search engines
;; and dictionaries on the internet to search for the keywords.  You 
;; shold specify which search engines you are going to use by 
;; `ndmisc-settings' variable.

;;; Code:

(require 'lookup-text)



;;;
;;; Public variable
;;;

(defvar ndmisc-settings
  '(
    ("Google (English)"	(ascii)
     "http://www.google.com/search?q=" utf-8)
    ("Google (日本語)"	(ascii japanese-jisx0208)
     "http://www.google.co.jp/search?q=" utf-8)
    ("Wikipedia (日本語)" (ascii japanese-jisx0208)
     "http://ja.wikipedia.org/wiki/" utf-8)
    ("Wikipedia (English)" (ascii)
     "http://en.wikipedia.org/wiki/" utf-8)
    ("2ch スレッドタイトル" (ascii japanese-jisx0208)
     "http://find.2ch.net/search?STR=" euc-jp "&SORT=MODIFIED")
    ("GlyphWiki" ndmisc-glyphwiki-charsetp
     "http://glyphwiki.org/wiki/" ndmisc-glyphwiki-encode)
    ("RFC" (ascii)
     "http://www.rfc-editor.org/cgi-bin/rfcsearch.pl?searchwords=" utf-8))
  "A set of lists of TITLE, CHARSETS, URL, ENCODING and URL-SUFFIX (optional).
CHARSET, URL and ENCODING may be a function.")

;;;
;;; Interface Functions
;;;

(put 'ndmisc :methods 'ndmisc-methods)
(defun ndmisc-methods (dictionary)
  '(exact))

(put 'ndmisc :list 'ndmisc-list)
(defun ndmisc-list (agent)
  (list (lookup-new-dictionary agent "misc")))

(put 'ndmisc :title 'ndmisc-title)
(defun ndmisc-title (dictionary)
  "Misc. Dict URLs")

(put 'ndmisc :search 'ndmisc-search)
(defun ndmisc-search (dictionary query)
  (list (lookup-new-entry 
         'regular dictionary 
         (lookup-query-pattern query))))

(put 'ndmisc :content 'ndmisc-content)
(defun ndmisc-content (entry)
  "Content String of ENTRY."
  (let ((string (lookup-entry-code entry))
        (item-no ?A)
        menu-items)
    (with-temp-buffer
      (insert string "\n")
      (dolist (entry ndmisc-settings)
        (let* ((title      (elt entry 0))
               (charsets   (elt entry 1))
               (url        (elt entry 2))
               (encoding   (elt entry 3))
               (url-suffix (elt entry 4)))
          (when (lookup-text-charsetsp string charsets)
            (setq menu-items
                  (cons (list item-no title
                              (concat (if (functionp url) (funcall url string)
                                        url)
                                      (ndmisc-encode-xwfu string encoding)
                                      url-suffix))
                        menu-items)
                  item-no (1+ item-no)))))
      (lookup-table-insert
       " → %c. %-10t     %-18t \n"
       (nreverse menu-items))
      (goto-char (point-min))
      (while (re-search-forward " +$" nil t) (replace-match ""))
      (buffer-string))))

(put 'ndmisc :arrange-table
     '((reference   ndmisc-jump-reference)))
(defun ndmisc-jump-reference (entry)
  (while (re-search-forward "\\(→ .\\.\\) \\(.+?\\)     \\(.+\\)" nil t)
    (let ((url (elt (get-text-property (1- (point)) 'lookup) 2)))
      (lookup-url-set-link (match-beginning 1) (match-end 1) url)
      (lookup-url-set-link (match-beginning 2) (match-end 2) url)
      (lookup-url-set-link (match-beginning 3) (match-end 3) url))))



;;;
;;; Utility Functions
;;;

;; exceprted from mm-url.el

(defconst ndmisc-url-unreserved-chars
  '(
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

(defun ndmisc-encode-xwfu (string encoding)
  "Escape characters in a STRING for application/x-www-form-urlencoded.
ENCODING will be used If ENCODING is function, then result of
applying function to the string will be returned."
  (if (coding-system-p encoding)
      (mapconcat
       (lambda (char)
         (cond
          ((= char ?  ) "+")
          ((memq char ndmisc-url-unreserved-chars) (char-to-string char))
          (t (upcase (format "%%%02x" char)))))
       (encode-coding-string string encoding)
       "")
    (if (functionp encoding) (funcall encoding string)
      (error "`encoding' is not valid coding-sytem"))))

;;;
;;; Individual Cases Functions
;;;

(defun ndmisc-glyphwiki-charsetp (string)
  (or (lookup-text-charsetsp string '(ascii))
      (string-match "^[⿰-⿻㐀-鿿豈-﫿𠀀-𯿿]+$" string)))

(defun ndmisc-glyphwiki-encode (string)
  (if (lookup-text-charsetsp string '(ascii)) string
    (mapconcat (lambda (x) (format "u%x" x)) string "-")))

(provide 'ndmisc)

;;; ndmisc.el ends here
