;;; mypaedia.el --- complement file for 『マイペディア９７』
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

(defconst mypaedia-gaiji-table
  (lookup-new-gaiji-table
   '(("ha121" ",A4") ("ha122" ",0,(B") ("ha123" ",07(B") ("ha124" ",0#(B") ("ha125" ",0'(B")
     ("ha126" ",0I") ("ha127" ",0U") ("ha128" ",0M") ("ha129" ",0r(B") ("ha12a" "`")
     ("ha12b" nil "1/2") ("ha12c" nil "1/3") ("ha12d" ",AA") ("ha12e" ",Aa")
     ("ha12f" ",A@") ("ha130" ",A`") ("ha133" ",AB") ("ha134" ",Ab") ("ha136" ",AD")
     ("ha137" ",Ad") ("ha138" ",AC") ("ha139" ",Ac") ("ha13a" ",AE") ("ha13b" ",Ae")
     ("ha13c" ",AF") ("ha13d" ",Af") ("ha154" ",Aj") ("ha157" ",AK") ("ha158" ",Ak")
     ("ha161" ",Am") ("ha162" ",AL") ("ha168" ",AO") ("ha169" ",Ao") ("ha16a" ",0!(B")
     ("ha16b" ",0Z") ("ha16e" ",B#") ("ha17a" ",AS") ("ha17b" ",As") ("ha17c" ",AR")
     ("ha17d" ",Ar") ("ha222" ",AT") ("ha223" ",At") ("ha224" ",AV") ("ha225" ",Av")
     ("ha226" ",AU") ("ha227" ",Au") ("ha228" ",AX") ("ha229" ",Ax") ("ha22e" ",01(B")
     ("ha22f" ",00(B") ("ha242" ",Az") ("ha243" ",AY") ("ha244" ",Ay") ("ha247" ",A\")
     ("ha248" ",A|") ("ha25b" ",0H") ("ha25c" ",0L")
     ("za121" nil ",A4") ("za122" nil ",0,(B") ("za123" nil ",07(B") ("za124" nil ",0#(B")
     ("za125" nil ",0'(B") ("za126" nil ",0I") ("za127" nil ",0U") ("za128" nil ",0M")
     ("za129" nil ",0r(B") ("za12a" nil "`") ("za12b" nil "1/2")
     ("za12c" nil "1/3") ("za12d" nil ",AA") ("za12e" nil ",Aa") ("za12f" nil ",A@")
     ("za130" nil ",A`") ("za133" nil ",AB") ("za134" nil ",Ab") ("za136" nil ",AD")
     ("za137" nil ",Ad") ("za138" nil ",AC") ("za139" nil ",Ac") ("za13a" nil ",AE")
     ("za13b" nil ",Ae") ("za13c" nil ",AF") ("za13d" nil ",Af") ("za154" nil ",Aj")
     ("za157" nil ",AK") ("za158" nil ",Ak") ("za161" nil ",Am") ("za162" nil ",AL")
     ("za168" nil ",AO") ("za169" nil ",Ao") ("za16a" nil ",0!(B") ("za16b" nil ",0Z")
     ("za16e" nil ",B#") ("za17a" nil ",AS") ("za17b" nil ",As") ("za17c" nil ",AR")
     ("za17d" nil ",Ar") ("za222" nil ",AT") ("za223" nil ",At") ("za224" nil ",AV")
     ("za225" nil ",Av") ("za226" nil ",AU") ("za227" nil ",Au") ("za228" nil ",AX")
     ("za229" nil ",Ax") ("za22e" nil ",01(B") ("za22f" nil ",00(B") ("za242" nil ",Az")
     ("za243" nil ",AY") ("za244" nil ",Ay") ("za247" nil ",A\") ("za248" nil ",A|")
     ("za25b" nil ",0H") ("za25c" nil ",0L"))))

(defconst mypaedia-reference-regexp
  (cond ((eq lookup-complement-agent 'ndtp)
	 "→\\(.*\\)<\\([0-9a-f:]+\\)>\n")
	((eq lookup-complement-agent 'ndeb)
	 "<reference>→\\(.*\\)</reference=\\([0-9a-f:]+\\)>\n")))

(defun mypaedia-arrange-references (entry)
  ;; この辞書はリンクがテキストの最後にまとめられているのだが、
  ;; 他の辞書に合わせて、テキストの途中に作る。
  (let ((dictionary (lookup-entry-dictionary entry))
	heading code start end)
    (while (re-search-forward mypaedia-reference-regexp nil t)
      (setq start (match-beginning 0) end (match-end 0))
      (setq heading (match-string 1) code (match-string 2))
      (setq entry (lookup-new-entry 'regular dictionary code heading))
      (delete-region start end)
      (if (search-backward (concat "⇒" heading) nil t)
	  (setq start (match-beginning 0) end (match-end 0))
	(insert "→" heading "\n")
	(setq end (1- (point))))
      (lookup-set-link start end entry))))

(setq lookup-complement-options
      (list ':gaiji-table mypaedia-gaiji-table
	    ':arrange-table '((reference . mypaedia-arrange-references))))

;;; mypaedia.el ends here
