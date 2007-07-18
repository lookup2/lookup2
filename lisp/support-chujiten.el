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
     ("ha124" . "°")
     ("ha125" . "˘")
     ("ha126" . "【")
     ("ha127" . "】")
     ("ha128" . "［")
     ("ha129" . "］")
     ("ha12a" . "~")
     ("ha12b" . "-")
     ("ha12c" . "-́")
     ("ha12d" . "-̀")
     ("ha132" . "˘")
     ("ha134" . "ç")
     ("ha135" . "ə́")
     ("ha136" . "ɚ́")
     ("ha137" . "í")
     ("ha138" . "ɔ́")
     ("ha139" . "ʊ́")
     ("ha13a" . "ɑ́")
     ("ha13b" . "´")
     ("ha13c" . "É")
     ("ha13d" . "á")
     ("ha13e" . "é")
     ("ha13f" . "í")
     ("ha140" . "ó")
     ("ha141" . "ú")
     ("ha142" . "ʌ́")
     ("ha143" . "ə̀")
     ("ha144" . "ɚ̀")
     ("ha145" . "ì")
     ("ha146" . "ɔ̀")
     ("ha147" . "ʊ̀")
     ("ha148" . "ɑ̀")
     ("ha149" . "`")
     ("ha14a" . "à")
     ("ha14b" . "è")
     ("ha14c" . "ì")
     ("ha14d" . "ò")
     ("ha14e" . "ù")
     ("ha14f" . "ʌ̀")
     ("ha150" . "ʌ")
     ("ha151" . "Á")
     ("ha152" . "B́")
     ("ha153" . "Ć")
     ("ha154" . "D́")
     ("ha155" . "É")
     ("ha156" . "F́")
     ("ha157" . "Ǵ")
     ("ha158" . "H́")
     ("ha159" . "Í")
     ("ha15a" . "Ĺ")
     ("ha15b" . "Ḿ")
     ("ha15c" . "Ó")
     ("ha15d" . "Ṕ")
     ("ha15e" . "Q́")
     ("ha15f" . "Ŕ")
     ("ha160" . "Ś")
     ("ha161" . "T́")
     ("ha162" . "Ú")
     ("ha163" . "V́")
     ("ha164" . "X́")
     ("ha165" . "Ý")
     ("ha166" . "Ź")
     ("ha167" . "á")
     ("ha168" . "é")
     ("ha169" . "í")
     ("ha16a" . "ó")
     ("ha16b" . "ú")
     ("ha16c" . "ý")
     ("ha16d" . "À")
     ("ha16e" . "È")
     ("ha16f" . "Ì")
     ("ha170" . "Ò")
     ("ha171" . "ǽ")
     ("ha172" . "")		; right half of [ae']
     ("ha173" . "æ̀")
     ("ha174" . "")
     ("ha175" . "æ")
     ("ha176" . "")
     ("ha177" . "S̀")
     ("ha178" . "T̀")
     ("ha179" . "Ù")
     ("ha17a" . "V̀")
     ("ha17b" . "à")
     ("ha17c" . "è")
     ("ha17d" . "ì")
     ("ha17e" . "ò")
     ("ha221" . "ù")
     ("ha222" . "ỳ")
     ("ha223" . "ɛ̃")
     ("ha224" . "ɔ̃")
     ("ha225" . "ɑ̃")
     ("ha226" . "ə")
     ("ha227" . "ɚ")
     ("ha228" . "ɪ")
     ("ha229" . "ɔ")
     ("ha22a" . "ʊ")
     ("ha22b" . "θ")
     ("ha22c" . "ð")
     ("ha22d" . "ʃ")
     ("ha22e" . "ʒ")
     ("ha22f" . "ŋ")
     ;; ("ha230")
     ("ha231" . "Φ")
     ;; ("ha232")
     ("ha233" . "ː")
     ("ha234" . "ɑ")
     ("ha235" . "ł")
     ("ha236" . "̃")
     ("ha237" . "ã")
     ("ha238" . "ñ")
     ("ha239" . "ø")
     ("ha23a" . "Å")
     ("ha23b" . "ţ")
     ("ha23c" . "°")
     ("ha23d" . "¨")
     ("ha23e" . "Ö")
     ("ha23f" . "ä")
     ("ha240" . "ë")
     ("ha241" . "ï")
     ("ha242" . "ö")
     ("ha243" . "ü")
     ("ha244" . "^")
     ("ha245" . "â")
     ("ha246" . "ê")
     ("ha247" . "î")
     ("ha248" . "ô")
     ("ha249" . "-")
     ("ha24a" . "ā")
     ("ha24b" . "ē")
     ("ha24c" . "ī")
     ("ha24d" . "ō")
     ("ha24e" . "ū")
     ("ha24f" . "ȳ")
     ("ha250" . "ă")
     ;; ("ha251" . "ĕ")
     ;; ("ha252" . "ŏ")
     ("ha253" . "Č")
     ("ha255" . "č")
     ;; ("ha256" . "e~")
     ("ha257" . "ľ")
     ("ha258" . "ř")
     ("ha259" . "š")
     ;; Any gaiji of the code "haxxx" after this doesn't seem to
     ;; appear except "ha26b"
     ("ha26a" . "˝")
     ("ha26b" . "·")
     ("ha26c" . "Ñ")
     ("ha26d" . "È")
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
     ("za34e" . "━")
     ("za34f" . "↔")
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
			     :reference-pattern))))

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
      (list :gaiji-table chujiten-gaiji-table
;;	    :reference-pattern 'chujiten-reference-pattern
	    :arrange-table chujiten-arrange-table
	    :transformer 'lookup-stemming-search))

;;; chujiten.el ends here
