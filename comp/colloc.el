;;; colloc.el --- complement file for 『新編 英和活用大辞典』
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

(defun colloc-arrange-first (entry)
  (if (re-search-forward " <reference>→<gaiji=zb12e>.*" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
  (when (looking-at "【.*】")
    (forward-line)
    (if (re-search-forward "^【.*】" nil t)
	(delete-region (match-beginning 0) (point-max)))
    (goto-char (point-min)))
  (if (re-search-forward " <reference>→<gaiji=zb12d>.*" nil t)
      (let ((string (match-string 0)))
	(goto-char (point-min))
	(end-of-line)
	(insert string)))
  (while (re-search-forward " <reference>→<gaiji=zb12d>.*" nil t)
    (delete-region (match-beginning 0) (match-end 0))))

(setq lookup-complement-options
      (list ':title "英和活用大辞典"
	    ':arrange-table '((replace . colloc-arrange-first))))

;;; colloc.el ends here
