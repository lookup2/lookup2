;;; vera.el --- support file for "V.E.R.A."
;; Copyright (C) 2000 Keisuke Nishida <knsihida@ring.gr.jp>

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

;;; Code:

(defun vera-arrange-references (entry)
  (when (search-forward "(" nil t)
    (let ((dictionary (lookup-entry-dictionary entry)) entry)
      (while (looking-at "[, ]*\\([^,)]+\\)")
	(setq entry (lookup-new-entry 'dynamic dictionary (match-string 1)))
	(lookup-put-property entry :dynamic 'lookup-dynamic-search)
	(lookup-set-link (match-beginning 1) (match-end 1) entry)
	(goto-char (match-end 0))))))

(setq lookup-support-options
      (list :title "V.E.R.A"
	    :arranges '((reference vera-arrange-references))))

;;; vera.el ends here
