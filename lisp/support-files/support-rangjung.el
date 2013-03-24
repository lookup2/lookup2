;;; support-rangjung.el --- support file for "Rangjung Yeshe" file.
;; Copyright (C) 2009 Lookup Development Team

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Documentation:

;; This support-file will search the Rangjung Yeshe Tibetan-English
;; Dictionary, distributed by Tibetan-Himalayan Library.

;; This dictionary can be purchased from the following site:
;; http://www.rangjung.com/ry-dic.htm

;;; Code:

(require 'tibet-util+)

;; Query-Filter
(defun support-rangjung-query-filter (query)
  (lookup-new-query-filter query 'tibetan-extended-wylie-encode-string))

;;; Arrangements
(defun support-rangjung-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (insert (lookup-entry-heading entry))
  (let ((heading 
         (replace-regexp-in-string " (.+)" "" (lookup-entry-heading entry))))
    (while (re-search-forward (concat "^" (regexp-quote heading) " ") nil t)
      (replace-match "")))
  (goto-char (point-min))
  (while (re-search-forward "{\\(.+?\\)}" nil t)
    (let ((marker (make-marker)))
      (set-marker marker (match-end 0))
      (goto-char (match-beginning 0))
      (insert (tibetan-extended-wylie-decode-string (match-string 1)) " ")
      (goto-char (marker-position marker)))))

(defun support-rangjung-head-tags (content)
  (when (string-match "^\\(.+?\\) - " content)
    (let ((head (match-string 1 content)))
      (concat head
              " (" 
              (tibetan-extended-wylie-decode-string head) 
              ")"))))

(setq lookup-support-options
      (list :title "Rangjung Yeshe"
            :query-filter 'support-rangjung-query-filter
            :content-tags '("\n" . "\n")
            :entry-tags '(nil . " - ")
            :code-tags '(nil . " - ")
            :arranges '((structure support-rangjung-arrange-structure))
            :head-tags 'support-rangjung-head-tags))

;;; support-rangjung.el ends here
