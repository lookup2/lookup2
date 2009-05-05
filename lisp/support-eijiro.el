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

(defvar support-eijiro-index-table
  '(("Eijiro112.dic" . "indices/dic00.idx")
    ("Waeiji112.dic" . "indices/dic01.idx")
    ("Ryaku112.dic" . "indices/dic02.idx")
    ("Reiji112.dic" . "indices/dic03.idx")))

(defvar support-eijiro-title-table
  '(("Eijiro112.dic" . "英辞郎")
    ("Waeiji112.dic" . "和英辞郎")
    ("Ryaku112.dic" . "略語辞典")
    ("Reiji112.dic" . "例辞郎")))

(defun support-eijiro-index-file (dict-id)
  (cdr 
   (assoc (replace-regexp-in-string "^.+/\\([^/]+\\)$" "\\1" dict-id)
          support-eijiro-index-table)))

(defun support-eijiro-title-name (dict-id)
  (cdr 
   (assoc (replace-regexp-in-string "^.+/\\([^/]+\\)$" "\\1" dict-id)
          support-eijiro-title-table)))

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

(let ((title (support-eijiro-title-name lookup-support-dictionary-id))
      (index (support-eijiro-index-file lookup-support-dictionary-id)))
  (message "Eijiro: dict-id=%s" lookup-support-dictionary-id)
  (message "Eijiro: index of %s is set to %s" title index)
  (setq lookup-support-options
        (list :title title
              :arranges '((reference support-eijiro-arrange-reference))
              :transformer 'lookup-stem-english-search
              :index index
              )))

;;; support-eijiro.el ends here
