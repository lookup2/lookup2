;;; mypaedia.el --- support file for 『マイペディア９７』 -*- coding: utf-8 -*-
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
   '(("ha121" "´") ("ha122" "ɑ") ("ha123" "ɔ") ("ha124" "ɛ") ("ha125" "ə")
     ("ha126" "ð") ("ha127" "ŋ") ("ha128" "ʒ") ("ha129" "ː") ("ha12a" "`")
     ("ha12b" nil "1/2") ("ha12c" nil "1/3") ("ha12d" "Á") ("ha12e" "á")
     ("ha12f" "À") ("ha130" "à") ("ha133" "Â") ("ha134" "â") ("ha136" "Ä")
     ("ha137" "ä") ("ha138" "Ã") ("ha139" "ã") ("ha13a" "Å") ("ha13b" "å")
     ("ha13c" "Æ") ("ha13d" "æ") ("ha154" "ê") ("ha157" "Ë") ("ha158" "ë")
     ("ha161" "í") ("ha162" "Ì") ("ha168" "Ï") ("ha169" "ï") ("ha16a" "ɪ")
     ("ha16b" "l") ("ha16e" "Ł") ("ha17a" "Ó") ("ha17b" "ó") ("ha17c" "Ò")
     ("ha17d" "ò") ("ha222" "Ô") ("ha223" "ô") ("ha224" "Ö") ("ha225" "ö")
     ("ha226" "Õ") ("ha227" "õ") ("ha228" "Ø") ("ha229" "ø") ("ha22e" "ɶ")
     ("ha22f" "œ") ("ha242" "ú") ("ha243" "Ù") ("ha244" "ù") ("ha247" "Ü")
     ("ha248" "ü") ("ha25b" "θ") ("ha25c" "ʃ")
     ("za121" nil "´") ("za122" nil "ɑ") ("za123" nil "ɔ") ("za124" nil "ɛ")
     ("za125" nil "ə") ("za126" nil "ð") ("za127" nil "ŋ") ("za128" nil "ʒ")
     ("za129" nil "ː") ("za12a" nil "`") ("za12b" nil "1/2")
     ("za12c" nil "1/3") ("za12d" nil "Á") ("za12e" nil "á") ("za12f" nil "À")
     ("za130" nil "à") ("za133" nil "Â") ("za134" nil "â") ("za136" nil "Ä")
     ("za137" nil "ä") ("za138" nil "Ã") ("za139" nil "ã") ("za13a" nil "Å")
     ("za13b" nil "å") ("za13c" nil "Æ") ("za13d" nil "æ") ("za154" nil "ê")
     ("za157" nil "Ë") ("za158" nil "ë") ("za161" nil "í") ("za162" nil "Ì")
     ("za168" nil "Ï") ("za169" nil "ï") ("za16a" nil "ɪ") ("za16b" nil "l")
     ("za16e" nil "Ł") ("za17a" nil "Ó") ("za17b" nil "ó") ("za17c" nil "Ò")
     ("za17d" nil "ò") ("za222" nil "Ô") ("za223" nil "ô") ("za224" nil "Ö")
     ("za225" nil "ö") ("za226" nil "Õ") ("za227" nil "õ") ("za228" nil "Ø")
     ("za229" nil "ø") ("za22e" nil "ɶ") ("za22f" nil "œ") ("za242" nil "ú")
     ("za243" nil "Ù") ("za244" nil "ù") ("za247" nil "Ü") ("za248" nil "ü")
     ("za25b" nil "θ") ("za25c" nil "ʃ"))))

(defconst mypaedia-reference-regexp
;  (cond ((eq lookup-support-agent 'ndtp)
;	 "→\\(.*\\)<\\([0-9a-f:]+\\)>\n")
;	((eq lookup-support-agent 'ndeb)
	 "<reference>→\\(.*\\)</reference=\\([0-9a-f:]+\\)>\n");))

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

(setq lookup-support-options
      (list :gaiji-table   mypaedia-gaiji-table
	    :arranges '((reference mypaedia-arrange-references))))

;;; mypaedia.el ends here
