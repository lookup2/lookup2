;;; iwakoku.el --- support file for 『岩波国語辞典』
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

(defconst iwakoku-gaiji-table
  (lookup-new-gaiji-table
   '(("za321" "[名]") ("za322" "[代]") ("za323" "[形]") ("za324" "[動]")
     ("za325" "[副]") ("za326" "[接]") ("za327" "[前]") ("za328" "[冠]")
     ("za329" "[間]") ("za32a" "[助") ("za32b" "動]") ("za32c" "[接")
     ("za32d" "頭]") ("za32e" "尾]") ("za32f" "[U]") ("za330" "[C]")
     ("za331" "(単)") ("za332" "(複)") ("za333" "[A]") ("za334" "[P]")
     ("za335" "(自)") ("za336" "(他)") ("za337" "[成") ("za338" "句]")
     ("za339" nil "[音]") ("za33a" nil "[例]") ("za33b" nil "[メモ]")
     ("za33c" nil "[一覧]") ("za37c" "(C)") ("za37d" "(R)") ("za722" "⇒"))))

(defconst iwakoku-arrange-table
  '((structure . iwakoku-arrange-headings)))

(defun iwakoku-fix-by-black-list (entry)
  (let ((code (lookup-entry-code entry)))
    (cond
     ((string= code "212d:d8")
      (search-forward "さ\nた") (replace-match "さた")))))

(defun iwakoku-arrange-headings (entry)
  (lookup-arrange-structure entry)
  (while (re-search-forward "\\(\\[.\\]\\)\\|\\(([0-9]+)\\)\\|([ア-ン]+)" nil t)
    (lookup-make-region-heading (match-beginning 0) (match-end 0)
				(if (match-beginning 1) 3
				  (if (match-beginning 2) 4 5)))
    (save-excursion
      (goto-char (match-beginning 0))
      (unless (or (bolp) (get-text-property (- (point) 2) 'lookup-heading))
	(newline)))))

(setq lookup-support-options
      (list ':gaiji-table iwakoku-gaiji-table
	    ':arrange-table iwakoku-arrange-table))

;;; iwakoku.el ends here
