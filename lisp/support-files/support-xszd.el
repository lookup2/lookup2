;;; support-xszd.el --- suport file for "學生字典" -*- coding: utf-8 -*-
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

;; This file support searching the "學生字典" (商務印書館 / 1915年版)
;; dictionary file.  The dictionary can be downloaded from the
;; following URL.
;;
;; http://kanji-database.cvs.sourceforge.net/viewvc/kanji-database/kanji-database/dict/xszd.txt
;;
;; Execute `mksary' program on "xszd.txt" file to generage
;; "xszd.txt.ary" suffix array file.

;;; Usage
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/xszd/")
;;           ....))

;;; Code:

(defvar support-xszd-use-ivs-font nil)

(defun support-xszd-arrange-structure (entry)
  "Attach contents of ENTRY a link and remove tags."
  (if (looking-at "\\*+")
      (replace-match ""))
  (goto-char (point-min))
  (let ((i ?①))
    (while (re-search-forward "^-\\([^-]\\)" nil t)
      (replace-match (concat (list i) (match-string 1)))
      (setq i (1+ i))))
  (goto-char (point-max))
  (if (looking-back "\\*+")
      (replace-match ""))
  (if support-xszd-use-ivs-font
      (lookup-text-new-to-old-kanji-ivs-region (point-min) (point-max))
  ))

(setq lookup-support-options
      (list :title "學生字典"
            :entry-tags   '("**" . "\n")
            :content-tags '("**" . "\n*")
            :coding 'utf-8
            :charsets '(han)
            :arranges '((replace support-xszd-arrange-structure))))

;;; support-xszd.el ends here
