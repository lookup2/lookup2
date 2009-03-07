;;; foldoc.el --- support file for "FOLDOC"
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

(require 'lookup)

(defconst foldoc-reference-pattern
  '("{\\([^}]+\\)}" 1 (lookup-oneline-string (match-string 1))
    lookup-dynamic-search))

(defun foldoc-arrange-replace (entry)
  (let ((heading (lookup-entry-heading entry)))
    (cond
     ((string= heading "control flow")
      (search-forward "structures}") (replace-match "structure}")))))

(setq lookup-support-options
      (list :title "FOLDOC"
	    :reference-pattern foldoc-reference-pattern
	    :arranges '((replace foldoc-arrange-replace))))

;;; foldoc.el ends here
