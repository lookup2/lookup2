;;; support-eijiro.el --- suport file for "Eijiro" -*- coding: utf-8 -*-
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

;; Index files for EIJIRO is put under `indices' directory.
;; This file provides index data for each dictionaries.

;;; Code:

(defvar support-eijiro-option-list
  '(("Eijiro112.dic" 
     "英辞郎"
     (ascii)
     "indices/dic00.idx")
    ("Waeiji112.dic" 
     "和英辞郎"
     (ascii japanese-jisx0208)
     "indices/dic01.idx")
    ("Ryaku112.dic" 
     "略語辞典"
     (ascii)
     "indices/dic02.idx")
    ("Reiji112.dic" 
     "例辞郎"
     (ascii japanese-jisx0208)
     "indices/dic03.idx")))

(defun support-eijiro-options (dict-id)
  (let* ((id (replace-regexp-in-string "^.+/\\([^/]+\\)$" "\\1" dict-id))
         (options (assoc id support-eijiro-option-list))
         (title    (elt options 1))
         (charsets (elt options 2))
         (index    (elt options 3)))
    `(:title    ,title
      :charsets ,charsets
      :query-filter lookup-query-filter-stem-english
      :arranges ((reference support-eijiro-arrange-reference))
      :index    ,index)))

(defun support-eijiro-arrange-reference (entry)
  (goto-char (point-min))
  (while (re-search-forward "<→\\(.+?\\)>" nil t)
    (support-eijiro-put-reference 
     (match-beginning 0) (match-end 0) (match-string 1)))
  (while (re-search-forward "◆file:\\(XXX.TXT\\)" nil t)
    (support-eijiro-insert-file-contents (match-string 1)))
  (while (re-search-forward "◆file:\\(YYY.HTM\\)" nil t)
    (support-eijiro-insert-file-contents (match-string 1)))
  (while (re-search-forward "◆file:\\(ZZZ.txt\\)" nil t)
    (support-eijiro-insert-file-contents (match-string 1)))
  )

(defvar dictionary nil) ;; exploit dynamically bound `dictionary' variable
(defun support-eijiro-put-reference (start end string)
  (when dictionary
    (let* ((entries (ndpdic-dictionary-search 
                    dictionary
                    (lookup-new-query 'exact string)))
           (entry (car entries)))
      (if entry (lookup-set-link start end entry)))))

(defun support-eijiro-insert-file-contents (file)
  ;; to be provided in future
  )

(let ((options (support-eijiro-options lookup-support-dictionary-id)))
  ;; (message "Eijiro: dict-id=%s" lookup-support-dictionary-id)
  ;; (message "Eijiro: index of %s is set to %s" (plist-get options :title) (plist-get options :index))
  (setq lookup-support-options options))

;;; support-eijiro.el ends here
