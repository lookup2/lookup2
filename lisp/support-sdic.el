;;; support-sdic.el --- support for "sdic" dictionary -*- coding: utf-8 -*-
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

;; This support file provides the ability to instantly search the
;; sdic-format dictionaries.  You will need to make the suffix array
;; index by "mksary".
;;

;;; Code:

(require 'lookup)
(require 'lookup-utils)

;;;
;;; Internal Constants
;;;

(defconst support-sdic-options-list
  '(("gene" 
     "Gene95")
    ("edict"
     "EDict"))
  "Assoc List for `:title' for each dictionaries.")

(defconst support-sdic-replace-entities
  '(
    ("amp" . "&")
    ("lt" . "<")
    ("gt" . ">")
    ("lf" . "\n")))

(defconst support-sdic-replace-entities-regexp
  (concat "&\\("
          (regexp-opt
           (mapcar 'car support-sdic-replace-entities))
          "\\);"))

(defun support-sdic-dictionary-options (dictionary-id)
  (let* ((dic-id (replace-regexp-in-string 
                   "^.+/\\([^/]+\\)\\.sdic$" "\\1" dictionary-id))
         (options (assoc dic-id support-sdic-options-list))
         (title   (elt options 1)))
    (list 
     :title title
     :charsets '(ascii japanese-jisx0208)
     :entry-tags '("<K>" . "</K>")
     :arranges '((replace   ndsdic-arrange-replace)))))

(defun ndsdic-arrange-replace (entry)
  "Arrange content of ENTRY."
  (goto-char (point-min))
  (if (looking-at "\n+") (replace-match ""))
  (while (re-search-forward ndsdic-replace-entities-regexp nil t)
    (replace-match
     (cdr (assoc (match-string 1) ndsdic-replace-entities))))
  (goto-char (point-min))
  (while (re-search-forward "^.*<H>\\(.+\\)</H>.*<K>.+</K>\\(.*\\)" nil t)
    (replace-match "\\1\n\\2\n" t))
  (goto-char (point-min))
  (while (re-search-forward "^.*<K>\\(.+\\)</K>\\(.*\\)" nil t)
    (replace-match "\\1\n\\2\n" t)))

(setq lookup-support-options
      (support-sdic-dictionary-options lookup-support-dictionary-id))

;;; support-sdic.el ends here
