;;; support-swjz.el --- suport file for "説文解字注" -*- coding: utf-8 -*-
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

;; This agent will search the the "説文解字注" XML dictionary file for
;; the head-word character.  The dictionary can be downloaded from the
;; following URL.
;;
;; http://kanji-database.sourceforge.net/dict/swjz/index.html
;;
;; Following Program will make index point file, which then can be
;; sorted by 'mksary -s' command.  

;;; Usage
;;
;;  * agent `location' must contain "swjz" and `:name' must be "char".
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/swjz/all.xml")
;;           ....))

;;; Code:

(defvar support-swjz-use-ivs-font t)

(defun support-swjz-arrange-structure (entry)
  "Attach contents of ENTRY a link and remove tags."
  (if support-swjz-use-ivs-font
      (lookup-text-new-to-old-kanji-ivs-region (point-min) (point-max)))
  (goto-char (point-min))
  (while (re-search-forward "<wordhead.+?</wordhead>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 2.0)) face lookup-heading-2-face)))
  (goto-char (point-min))
  (while (re-search-forward "<explanation.+?</explanation>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 1.5)) face lookup-heading-3-face)))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (search-forward "	" nil t) (replace-match ""))
  (goto-char (point-min))
  (if (looking-at "$") (delete-region (point) (1+ (point)))))

(setq lookup-support-options
      (list :title "説文解字注"
            :entry-tags '("\">" . "</wordhead>")
            :code-tags '(" id=\"" . "\" ")
            :content-tags '("<shuowen>" . "</shuowen>")
            :coding 'utf-8-dos
            :charsets '(han)
            :arranges '((replace support-swjz-arrange-structure))))

;;; support-swjz.el ends here
