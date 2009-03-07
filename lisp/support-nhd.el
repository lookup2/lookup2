;;; nhd.el --- support file for "The Newbury House Dicitonary"
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

(defun nhd-arrange-structure (entry)
  (nhd-arrange-structure-internal "b" 'bold)
  (nhd-arrange-structure-internal "i" 'italic)
  (nhd-arrange-structure-internal "j" 'lookup-heading-low-face))

(defun nhd-arrange-structure-internal (tag face)
  (let ((start (format "<%s>" tag))
	(end (format "</%s>" tag)))
    (goto-char (point-min))
    (while (re-search-forward start nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (let ((start (point)))
	(when (re-search-forward end nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (put-text-property start (point) 'face face))))))

(setq lookup-support-options
      (list :title "Newbury House"
	    :arranges '((structure nhd-arrange-structure))))

;;; jargon.el ends here
