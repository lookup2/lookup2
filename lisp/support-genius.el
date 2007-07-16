;;; genius.el --- support file for 『ジーニアス英和・和英辞典』
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
   '(("ha121" . ",Aa")
     ("ha122" . ",A`")
     ("ha123" . ",0,(B")
     ("ha124" . ",0,(B'")
     ("ha125" . ",0,(B`")
     ("ha127" . ",0+(B")
     ("ha12a" . ",0'(B")
     ("ha12b" . ",0'(B'")
     ("ha12e" . ",Ai")
     ("ha12f" . ",Ah")
     ("ha134" . ",Am")
     ("ha135" . ",Al")
     ("ha136" . ",As")
     ("ha137" . ",Ar")
     ("ha13a" . ",07(B")
     ("ha13e" . ",Az")
     ("ha13f" . ",Ay")
     ("ha143" . ",0$(B")
     ("ha149" . "g")
     ("ha14a" . ",0U")
     ("ha14b" . ",03(B-")
     ("ha14c" . ",0I")
     ("ha14d" . ",0L")
     ("ha152" . ",0M")
     ("ha154" . ",0r(B")
     ("ha155" . ",Aa")
     ("ha156" . ",A`")
     ("ha157" . ",Ai")
     ("ha158" . ",Ah")
     ("ha159" . ",Am")
     ("ha15a" . ",Al")
     ("ha15b" . ",As")
     ("ha15c" . ",Ar")
     ("ha15d" . ",Az")
     ("ha15e" . ",Ay")
     ("ha161" . ",Ag")
     ("ha16a" . ",AA")
     ("ha16b" . ",A@")
     ("ha16d" . ",AI")
     ("ha16e" . ",AS")
     ("ha16f" . ",AR")
     ("ha171" . ",A}")
     ("ha172" . "y`")
     ("ha176" . ",AM")
     ("ha235" . "/")
     ("za430" . "[C]")
     ("za431" . "[U]")
     ("za432" . "[S]")
     ("za433" . "[D]")
     ("za43a" . "−"))))

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
  (when (re-search-forward "\\`\\([^*/]+\\)\\(\\**\\)?/[^/]+/" nil t)
    (newline)
    (let ((mb2 (match-beginning 2)) (me2 (match-end 2)))
      (lookup-make-region-heading (point-min) (match-end 1) 1)
      (when (< mb2 me2)
	(let ((num (length (buffer-substring mb2 me2))))
	  (delete-region mb2 me2)
	  (goto-char (point-min))
	  (insert "(")
	  (insert-char ?+ num)
	  (insert ")")))))
  ;; level 2-6
  (let ((case-fold-search nil) n)
    (while (re-search-forward genius-structure-regexp nil t)
      (setq n 1)
      (while (<= n 6)
	(if (match-beginning n)
	    (lookup-make-region-heading
	     (match-beginning n) (match-end n) (1+ n)))
	(setq n (1+ n))))))

(setq lookup-support-options
      (list :gaiji-table genius-gaiji-table
	    :arrange-table '((structure . genius-arrange-structure))
	    :transformer 'lookup-stemming-search))

;;; genius.el ends here
