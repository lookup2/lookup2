;;; support-eijiro-pdic.el --- suport file for "Eijiro" -*- coding: utf-8 -*-
;; Copyright (C) 2009 KAWABATA, Taichi <kawabata.taichi@gmail.com>

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

;;; Documentation

;;; Code:

(require 'lookup)
(eval-when-compile (require 'cl))
(require 'cl-lib)

(defvar support-eijiro-pdic-option-list
  '(("EIJI.*\\.DIC" 
     "英辞郎"
     (ascii)
     "indices/dic00.idx")
    ("WAEI.*\\.DIC" 
     "和英辞郎"
     (ascii japanese-jisx0208)
     "indices/dic01.idx")
    ("RYAKU.*\\.DIC" 
     "略語辞典"
     (ascii)
     "indices/dic02.idx")
    ("REIJI.*\\.DIC" 
     "例辞郎"
     (ascii japanese-jisx0208)
     "indices/dic03.idx")))

(defun support-eijiro-pdic-options (dict-id)
  (multiple-value-bind
      (regexp title charsets index)
      (cl-assoc-if (lambda (x) (string-match x dict-id))
                   support-eijiro-pdic-option-list)
    `(:title    ,title
      :charsets ,charsets
      :query-filter lookup-query-filter-stem-english
      ; :arranges ((reference support-eijiro-pdic-arrange-reference))
      :index    ,index)))

;(defun support-eijiro-pdic-arrange-reference (entry)
;  (goto-char (point-min))
;  (while (re-search-forward "<→\\(.+?\\)>" nil t)
;    (support-eijiro-pdic-put-reference 
;     (match-beginning 0) (match-end 0) (match-string 1)))
;  (while (re-search-forward "◆file:\\(XXX.TXT\\)" nil t)
;    (support-eijiro-pdic-insert-file-contents (match-string 1)))
;  (while (re-search-forward "◆file:\\(YYY.HTM\\)" nil t)
;    (support-eijiro-pdic-insert-file-contents (match-string 1)))
;  (while (re-search-forward "◆file:\\(ZZZ.txt\\)" nil t)
;    (support-eijiro-pdic-insert-file-contents (match-string 1))))
;
;(defvar dictionary nil)
;(defun support-eijiro-pdic-put-reference (start end string)
;  (when dictionary
;    (let* ((entries (ndpdic-dictionary-search 
;                    dictionary
;                    (lookup-new-query 'exact string)))
;           (entry (car entries)))
;      (if entry (lookup-set-link start end entry)))))

(setq lookup-support-options
      (support-eijiro-pdic-options lookup-support-dictionary-id))

;;; support-eijiro-pdic.el ends here
