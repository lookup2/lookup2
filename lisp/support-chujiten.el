;;; chujiten.el --- support file for 『新英和・和英中辞典』
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
     ;; ("ha125")
     ("ha126" . "【")
     ("ha127" . "】")
     ("ha128" . "［")
     ("ha129" . "］")
     ("ha12a" nil "~")
     ("ha12b" . "-")
     ("ha12c" . "-'")
     ("ha12d" . "-`")
     ;; ("ha12e") - ("ha133")
     ("ha134" . ",Ag")
     ("ha135" . ",0'(B'")
     ("ha136" . ",0:(B'")
     ("ha137" . ",0!(B'")
     ("ha138" . ",07(B'")
     ("ha139" . ",05(B'")
     ("ha13a" . ",0,(B'")
     ("ha13b" . "'")
     ("ha13c" . "E'")
     ("ha13d" . ",Aa")
     ("ha13e" . ",Ai")
     ("ha13f" . ",Am")
     ("ha140" . ",As")
     ("ha141" . ",Az")
     ("ha142" . ",0+(B'")
     ("ha143" . ",0'(B`")
     ("ha144" . ",0:(B`")
     ("ha145" . ",0!(B`")
     ("ha146" . ",07(B`")
     ("ha147" . ",05(B`")
     ("ha148" . ",0,(B`")
     ("ha149" . "`")
     ("ha14a" . ",A`")
     ("ha14b" . ",Ah")
     ("ha14c" . ",Al")
     ("ha14d" . ",Ar")
     ("ha14e" . ",Ay")
     ("ha14f" . ",0+(B`")
     ("ha150" . ",0+(B")
     ("ha151" . "A'")
     ("ha152" . "B'")
     ("ha153" . "C'")
     ("ha154" . "D'")
     ("ha155" . "E'")
     ("ha156" . "F'")
     ("ha157" . "G'")
     ("ha158" . "H'")
     ("ha159" . "I'")
     ("ha15a" . "L'")
     ("ha15b" . "M'")
     ("ha15c" . "O'")
     ("ha15d" . "P'")
     ("ha15e" . "Q'")
     ("ha15f" . "R'")
     ("ha160" . "S'")
     ("ha161" . "T'")
     ("ha162" . "U'")
     ("ha163" . "V'")
     ("ha164" . "X'")
     ("ha165" . "Y'")
     ("ha166" . "Z'")
     ("ha167" ",Aa" "a")
     ("ha168" ",Ai" "e")
     ("ha169" ",Am" "i")
     ("ha16a" ",As" "o")
     ("ha16b" ",Az" "u")
     ("ha16c" ",A}" "y")
     ("ha16d" . "A`")
     ("ha16e" . "E`")
     ("ha16f" . "I`")
     ("ha170" . "O`")
     ("ha171" . ",0$(B'")
     ("ha172" . "")
     ("ha173" . ",0$(B`")
     ("ha174" . "")
     ("ha175" . ",0$(B")
     ("ha176" . "")
     ("ha177" . "S`")
     ("ha178" . "T`")
     ("ha179" . "U`")
     ("ha17a" . "V`")
     ("ha17b" ",A`" "a")
     ("ha17c" ",Ah" "e")
     ("ha17d" ",Al" "i")
     ("ha17e" ",Ar" "o")
     ("ha221" ",Ay" "u")
     ("ha222" "y`" "y")
     ("ha223" . ",0;(B")
     ("ha224" . ",0>(B")
     ("ha225" . ",0<(B")
     ("ha226" . ",0'(B")
     ("ha227" . ",0:(B")
     ("ha228" . ",0!(B")
     ("ha229" . ",07(B")
     ("ha22a" . ",05(B")
     ("ha22b" . ",0H")
     ("ha22c" . ",0I")
     ("ha22d" . ",0L")
     ("ha22e" . ",0M")
     ("ha22f" . ",0U")
     ;; ("ha22f")
     ;; ("ha230")
     ;; ("ha231")
     ;; ("ha232")
     ("ha233" . ",0r(B")
     ("ha234" . ",0,(B")
     ("ha235" . ",B3")
     ("ha236" . "~")
     ("ha237" . ",Ac")
     ("ha238" . ",Aq")
     ("ha239" . ",Ax")
     ("ha23a" . ",AE")
     ("ha23b" . ",B~")
     ("ha23c" . ",A0")
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
     ("ha249" . "-")
     ("ha24a" . ",D`")
     ("ha24b" . ",D:")
     ("ha24c" . ",Do")
     ("ha24d" . ",Dr")
     ("ha24e" . ",D~")
     ("ha24f" . "y-")
     ("ha250" . ",Bc")
     ;; ("ha251" . "e~")
     ;; ("ha252" . "o~")
     ("ha253" . ",BH")
     ;; ("ha253" . "a~")
     ("ha255" . ",Bh")
     ;; ("ha256" . "e~")
     ("ha257" . ",B5")
     ("ha258" . ",Bx")
     ("ha259" . ",B9")
     ;; Any gaiji of the code "haxxx" after this doesn't seem to
     ;; appear except "ha26b", so we don't define them.
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
     ("za339" . "[音]")
     ("za33a" . "[例]")
     ("za33b" . "[メモ]")
     ("za33c" . "[一覧]")
     ("za33f" . "→")
     ("za34e" . "−")
     ("za34f" . "⇔")
     ("za722" . "⇒"))))

;; reference pattern

(defconst chujiten-base-reference-regexp
  (cond ((eq lookup-support-agent 'ndtp)
	 "→<\\([0-9a-f:]+\\)>")
	((eq lookup-support-agent 'ndeb)
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
;   (cond ((eq lookup-support-agent 'ndtp)
; 	 "→<gaiji:za33a><\\([0-9a-f:]+\\)>")
; 	((eq lookup-support-agent 'ndeb)
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

;; support options

(setq lookup-support-options
      (list ':gaiji-table chujiten-gaiji-table
	    ':reference-pattern 'chujiten-reference-pattern
	    ':arrange-table chujiten-arrange-table
	    ':transformer 'lookup-stemming-search))

;;; chujiten.el ends here
