;;; chujiten.el --- complement file for 『新英和・和英中辞典』
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

(defconst chujiten-gaiji-table
  (lookup-new-gaiji-table
   '(("ha121" . "(+)")
     ("ha122" . "(++)")
     ("ha123" . "(+++)")
     ("ha124" . "(*)")
     ("ha126" . "【")
     ("ha127" . "】")
     ("ha128" . "[")
     ("ha129" . "]")
     ("ha12a" nil "~")
     ("ha12b" nil "-")
     ("ha12c" nil "-'")
     ("ha12d" nil "-`")
     ("ha134" . ",Ag")
     ("ha135" nil "e/'")
     ("ha136" nil "e/|'")
     ("ha137" . ",Am")
     ("ha138" nil "/c'")
     ("ha139" nil "U-'")
     ("ha13a" nil "a|'")
     ("ha13b" . [intern "'"])
     ("ha13c" nil "E'")
     ("ha13d" . ",Aa")
     ("ha13e" . ",Ai")
     ("ha13f" . ",Am")
     ("ha140" . ",As")
     ("ha141" . ",Az")
     ("ha142" nil "/v'")
     ("ha143" nil "e/`")
     ("ha144" nil "e/`")
     ("ha145" nil "I`")
     ("ha146" nil "c/`")
     ("ha147" nil "U-`")
     ("ha148" nil "a|`")
     ("ha149" nil "`")
     ("ha14a" . ",A`")
     ("ha14b" . ",Ah")
     ("ha14c" . ",Al")
     ("ha14d" . ",Ar")
     ("ha14e" . ",Ay")
     ("ha14f" nil "/v`")
     ("ha150" . [intern "/v"])
     ("ha151" . [intern "A'"])
     ("ha152" . [intern "B'"])
     ("ha153" . [intern "C'"])
     ("ha154" . [intern "D'"])
     ("ha155" . [intern "E'"])
     ("ha156" . [intern "F'"])
     ("ha157" . [intern "G'"])
     ("ha158" . [intern "H'"])
     ("ha159" . [intern "I'"])
     ("ha15a" . [intern "L'"])
     ("ha15b" . [intern "M'"])
     ("ha15c" . [intern "O'"])
     ("ha15d" . [intern "P'"])
     ("ha15e" . [intern "Q'"])
     ("ha15f" . [intern "R'"])
     ("ha160" . [intern "S'"])
     ("ha161" . [intern "T'"])
     ("ha162" . [intern "U'"])
     ("ha163" . [intern "V'"])
     ("ha164" . [intern "X'"])
     ("ha165" . [intern "Y'"])
     ("ha166" . [intern "Z'"])
     ("ha167" . [intern "a'"])
     ("ha168" . [intern "e'"])
     ("ha169" . [intern "i'"])
     ("ha16a" . [intern "o'"])
     ("ha16b" . [intern "u'"])
     ("ha16c" . [intern "y'"])
     ("ha16d" . [intern "A`"])
     ("ha16e" . [intern "E`"])
     ("ha16f" . [intern "I`"])
     ("ha170" . [intern "O`"])
     ("ha171" . [intern "ae'"])
     ("ha172" . "")
     ("ha173" . [intern "ae`"])
     ("ha174" . "")
     ("ha175" . [intern "ae"])
     ("ha176" . "")
     ("ha177" nil "S")
     ("ha178" nil "T")
     ("ha179" . [intern "U`"])
     ("ha17a" nil "V")
     ("ha17b" . [intern "a`"])
     ("ha17c" . [intern "e`"])
     ("ha17d" . [intern "i`"])
     ("ha17e" . [intern "o`"])
     ("ha221" . [intern "u`"])
     ("ha222" . [intern "y`"])
     ("ha223" . [intern "/3~"])
     ("ha224" . [intern "/c~"])
     ("ha225" . [intern "A~"])
     ("ha226" . [intern "/e"])
     ("ha227" . [intern "e|"])
     ("ha228" . [intern "I"])
     ("ha229" . [intern "/c"])
     ("ha22a" . [intern "U"])
     ("ha22b" . [intern "o-"])
     ("ha22c" . [intern "dh"])
     ("ha22d" . [intern "sh"])
     ("ha22e" . [intern "zh"])
     ("ha22f" . [intern "ng"])
     ("ha233" . [intern ":"])
     ("ha234" . [intern "A"])
     ("ha235" . [intern "l/"])
     ("ha236" . "~")
     ("ha237" . [intern "a~"])
     ("ha238" . [intern "n~"])
     ("ha239" . [intern "o/"])
     ("ha23a" . ",AE")
     ("ha23b" . ",B~")
     ("ha23c" nil "*")
     ("ha23d" . ",A(")
     ("ha23e" . ",AV")
     ("ha23f" . ",Ad")
     ("ha240" . ",Ak")
     ("ha241" . ",Ao")
     ("ha242" . ",Av")
     ("ha243" . ",A|")
     ("ha244" . "^")
     ("ha245" . ",Ab")
     ("ha246" . ",Aj")
     ("ha247" . ",An")
     ("ha248" . ",At")
     ("ha249" nil "=")
     ("ha24a" nil "=a")
     ("ha24b" . "=e")
     ("ha24c" . "=i")
     ("ha24d" . "=o")
     ("ha24e" . "=u")
     ("ha24f" . "=y")
     ("ha250" . ",Bc")
     ("ha253" . ",BH")
     ("ha255" . ",Bh")
     ("ha257" . ",B5")
     ("ha258" . ",Bx")
     ("ha259" . ",B9")
     ("ha26b" . "*")
     ("za321" . "[名]")
     ("za322" . "[代]")
     ("za323" . "[形]")
     ("za324" . "[動]")
     ("za325" . "[副]")
     ("za326" . "[接]")
     ("za327" . "[前]")
     ("za328" . "[冠]")
     ("za329" . "[間]")
     ("za32a" . "[助")
     ("za32b" . "動]")
     ("za32c" . "[接")
     ("za32d" . "頭]")
     ("za32e" . "尾]")
     ("za32f" . "[U]")
     ("za330" . "[C]")
     ("za331" . "(単)")
     ("za332" . "(複)")
     ("za333" . "[A]")
     ("za334" . "[P]")
     ("za335" . "(自)")
     ("za336" . "(他)")
     ("za337" . "[成")
     ("za338" . "句]")
     ("za339" nil "[音]")
     ("za33a" nil "[例]")
     ("za33b" nil "[メモ]")
     ("za33c" nil "[一覧]")
     ("za33f" . "⇒")
     ("za34e" . "−")
     ("za34f" . "⇔")
     ("za37c" . "(C)")
     ("za37d" . "(R)")
     ("za722" . "⇒"))))

;; reference pattern

(defconst chujiten-base-reference-regexp
  (cond ((eq lookup-complement-agent 'ndtp)
	 "→<\\([0-9a-f:]+\\)>")
	((eq lookup-complement-agent 'ndeb)
	 "<reference>→</reference=\\([0-9a-f:]+\\)>")))

(defconst chujiten-eiwa-reference-pattern
  (list (concat chujiten-base-reference-regexp "\\([a-zA-Z' ]*[０-９]*\\>\\)?")
	'(concat "→" (match-string 2)) 2 1))

(defconst chujiten-waei-reference-pattern
  (list (concat chujiten-base-reference-regexp "\\([^ ,.\n]*\\)?")
	'(concat "→" (match-string 2)) 2 1))

(defun chujiten-reference-pattern (entry)
  (cond
   ((chujiten-eiwa-entry-p entry) chujiten-eiwa-reference-pattern)
   ((chujiten-waei-entry-p entry) chujiten-waei-reference-pattern)
   (t (lookup-dictionary-ref (lookup-entry-dictionary entry)
			     ':reference-pattern))))

;; arrange table

(defconst chujiten-arrange-table
  '((structure . chujiten-arrange-structure)))

; (defconst chujiten-example-regexp
;   (cond ((eq lookup-complement-agent 'ndtp)
; 	 "→<gaiji:za33a><\\([0-9a-f:]+\\)>")
; 	((eq lookup-complement-agent 'ndeb)
; 	 "<reference>→<gaiji=za33a></reference=\\([0-9a-f:]+\\)>")))

; (defun chujiten-arrange-expand-examples (entry)
;   (setq entry (lookup-new-entry (lookup-entry-dictionary entry) nil ""))
;   (while (re-search-forward chujiten-example-regexp nil t)
;     (lookup-entry-set-code entry (match-string 1))
;     (delete-region (match-beginning 0) (match-end 0))
;     (forward-line)
;     (narrow-to-region (point) (progn (insert (lookup-dictionary-command
; 					      dictionary 'content entry))
; 				     (point)))
;     (goto-char (point-min))
;     (while (not (eobp)) (insert "*") (forward-line))
;     (widen)))

(defconst chujiten-eiwa-structure-regexp
  (concat "^\\(−\\[[^]\n]+\\]\\)\\|"		; level 2
	  "^\\([A-Z]\\>\\)\\|"			; level 3
	  "^\\([0-9]+\\)?\\([a-z]\\)?\\>\\|"	; level 4, 5
	  "^\\(\\*.*\n\\)"))			; level 6

(defun chujiten-eiwa-arrange-structure (entry)
  ;; 見出し語を level 1
  (when (looking-at "\\(([+*]+)\\)?\\([^/\n]*\\) *\\(/[^/\n]+/\\)?")
    (lookup-make-region-heading (match-beginning 2) (match-end 2) 1))
  (forward-line)
  ;; level 2-6
  (let ((case-fold-search nil) n)
    (while (re-search-forward chujiten-eiwa-structure-regexp nil t)
      (setq n 1)
      (while (<= n 6)
	(if (match-beginning n)
	    (lookup-make-region-heading
	     (match-beginning n) (match-end n) (1+ n)))
	(setq n (1+ n))))))

(defun chujiten-waei-arrange-structure (entry)
  (lookup-make-region-heading (point) (progn (end-of-line) (point)) 1)
  (forward-line)
  (while (re-search-forward "^\\([0-9]+\\)\\|^\\(\\(【文例】\\)?\\*.*\n\\)" nil t)
    (if (match-beginning 1)
	(lookup-make-region-heading (match-beginning 1) (match-end 0) 4)
      (lookup-make-region-heading (match-beginning 2) (match-end 2) 6))))

(defun chujiten-arrange-structure (entry)
  (cond
   ((chujiten-eiwa-entry-p entry) (chujiten-eiwa-arrange-structure entry))
   ((chujiten-waei-entry-p entry) (chujiten-waei-arrange-structure entry))
   (t (lookup-arrange-structure entry))))

;; internal functions

(defun chujiten-eiwa-entry-p (entry)
  (let ((code (lookup-entry-code entry)))
    (and (string< "17a2" code) (string< code "6e8d"))))

(defun chujiten-waei-entry-p (entry)
  (let ((code (lookup-entry-code entry)))
    (and (string< "6e8d" code) (string< code "a773"))))

(defun chujiten-menu-entry-p (entry)
  (let ((code (lookup-entry-code entry)))
    (or (string< code "17a2") (string< "a773" code))))

;; complement options

(setq lookup-complement-options
      (list ':gaiji-table chujiten-gaiji-table
	    ':reference-pattern 'chujiten-reference-pattern
	    ':arrange-table chujiten-arrange-table))

;;; chujiten.el ends here
