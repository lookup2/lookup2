;;; genius.el --- complement file for 『ジーニアス英和・和英辞典』
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

(defconst genius-gaiji-table
  (lookup-new-gaiji-table
   '(("ha121" . [intern "a'"])
     ("ha122" . [intern "a`"])
     ("ha123" . [intern "A"])
     ("ha124" nil "a")
     ("ha125" nil "a")
     ("ha127" . [intern "/v"])
     ("ha12a" . [intern "/e"])
     ("ha12e" . [intern "e'"])
     ("ha12f" . [intern "e`"])
     ("ha134" . [intern "i'"])
     ("ha135" . [intern "i`"])
     ("ha136" . [intern "o'"])
     ("ha137" . [intern "o`"])
     ("ha13a" . [intern "/c"])
     ("ha13e" . [intern "u'"])
     ("ha13f" . [intern "u`"])
     ("ha143" . [intern "ae"])
     ("ha149" . [intern "g"])
     ("ha14a" . [intern "ng"])
     ("ha14b" . [intern "o-"])
     ("ha14c" . [intern "dh"])
     ("ha14d" . [intern "sh"])
     ("ha152" . [intern "zh"])
     ("ha154" . [intern ":"])
     ("ha155" . [intern "a'"])
     ("ha156" . [intern "a`"])
     ("ha157" . [intern "e'"])
     ("ha158" . [intern "e`"])
     ("ha159" . [intern "i'"])
     ("ha15a" . [intern "i`"])
     ("ha15b" . [intern "o'"])
     ("ha15c" . [intern "o`"])
     ("ha15d" . [intern "u'"])
     ("ha15e" . [intern "u`"])
     ("ha161" . [intern "c,"])
     ("ha16a" . [intern "A'"])
     ("ha16b" . [intern "A`"])
     ("ha16d" . [intern "E'"])
     ("ha16e" . [intern "O'"])
     ("ha16f" . [intern "O`"])
     ("ha171" . [intern "y'"])
     ("ha172" . [intern "y`"])
     ("ha176" . [intern "I'"])
     ("ha235" "/")
     ("za430" "[C]")
     ("za431" "[U]")
     ("za432" "[S]")
     ("za433" "[D]")
     ("za43a" "−"))))

(defconst genius-structure-regexp
  (concat "^\\(−【.*】\\)\\|"			; level 2
	  "^\\(−｜.*｜\\)\\|"			; level 3
	  "^\\([0-9]+\\)\\(\\)\\|"		; level 4
	  "^\\(\\*.*\n\\)"))			; level 6

(defun genius-arrange-structure (entry)
  ;; break examples into lines
  (save-restriction
    (while (search-forward "‖" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert "\n*")
      (narrow-to-region (point) (progn (end-of-line) (point)))
      (goto-char (point-min))
      (while (search-forward "/" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(insert "\n*"))
      (widen)))
  ;; level 1
  (goto-char (point-min))
  (if (re-search-forward "\\`[^/]+/[^/]+/" nil t)
      (newline))
  (goto-char (point-min))
  (lookup-arrange-structure entry)
  ;; level 2-6
  (let ((case-fold-search nil) n)
    (while (re-search-forward genius-structure-regexp nil t)
      (setq n 1)
      (while (<= n 6)
	(if (match-beginning n)
	    (lookup-make-region-heading
	     (match-beginning n) (match-end n) (1+ n)))
	(setq n (1+ n))))))

(setq lookup-complement-options
      (list ':gaiji-table genius-gaiji-table
	    ':arrange-table '((structure . genius-arrange-structure))))

;;; genius.el ends here
