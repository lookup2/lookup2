;;; support-crown.el --- suport file for "Crown French/German-Japanese Dictionary"
;; Copyright (C) 2000 KAWABATA Taichi <kawabata@sourceforge.net>

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

(let 
  ((encoded-crown-gaiji-table
    '(
      ("ha121" "Ç")
      ("ha122" "œ")
      ("ha123" "ç")
      ("ha124" "â")
      ("ha125" "ã")
      ("ha126" "Ȗ")
      ("ha127" "ə")
      ("ha128" "ɔ")
      ("ha129" "ɛ")
      ("ha12a" "ɡ")
      ("ha12b" "ɲ")
      ("ha12c" "ŋ")
      ("ha12d" "ø")
      ("ha12e" "ʃ")
      ("ha12f" "ʒ")
      ("ha130" "ː")
      ("ha131" "ɑ̀")
      ("ha132" "ɑ̃")
      ("ha133" "æ̃")
      ("ha134" "ɔ̃")
      ("ha135" "ɛ̃")
      ("ha136" "ɥ")
      ;; ??
      ("ha137" "ˏ")
      ("ha138" "ī")
      ("ha139" "ñ")
      ("ha13a" "õ")
      ("ha13b" "œ̃")
      ("ha13c" "É")
      ("ha13d" "á")
      ("ha13e" "é")
      ("ha13f" "´")
      ("ha140" "À")
      ("ha141" "È")
      ("ha142" "à")
      ("ha143" "è")
      ("ha144" "ù")
      ("ha145" "`")
      ("ha146" "ä")
      ("ha147" "ë")
      ("ha148" "ö")
      ("ha149" "ü")
      ("ha14a" "¨")
      ("ha14b" "ï")
      ("ha14c" "Â")
      ("ha14d" "Ê")
      ("ha14e" "Ô")
      ("ha14f" "ê")
      ("ha150" "î")
      ("ha151" "ô")
      ("ha152" "û")
      ("ha153" "^")
      ("ha154" "î")
      ("ha155" "ℊ")
      ("ha156" "æ")
      ("ha157" "ı")
      ("ha158" "β")
      ("ha159" "í")
      ("ha15a" "ó")
      ("ha15b" "ú")
      ("ha15c" "ă")
      ;; caron or breve?
      ("ha15d" "ǐ")
      ("ha15e" "ŭ")
      ("ha15f" "Θ")
      ("ha160" "ʌ")
      ("ha161" "ò")
      ("ha162" "ɑ́")
      ("ha163" "ǽ")
      ("ha164" "ə́")
      ("ha165" "ɔ́")
      ("ha166" "ɛ́")
      ("ha167" "ʜ́")
      ("ha168" "-́")
      ("ha169" "ɪ")
      ;; small letter U, V.
      ("ha16a" "U")
      ("ha16b" "V")
      ("ha16c" "ʏ")
      ("ha16d" "ℓ")
      ;; delta sign?
      ("ha16e" "/")
      ;; ??
      ("ha16f" ";")
      ("ha170" "-")
      ;; ?
      ("ha171" "+")
      ("ha172" "ṍ")
      ("ha173" "Ã́")
      ("ha174" "ɛ̃́")
      ("ha175" "ɔ̃́")
      ("ha176" "ɐ")
      ("ha177" "ɐ̌")
      ("ha178" "ñ")
      ("ha179" "ɐ̃")
      ("ha17a" "Č")
      ("ha17b" "č")
      ("ha17c" "Ž")
      ;; caron or breve??
      ("ha17d" "y̆")
      ;; caron or breve??
      ("ha17e" "ľ")
      ("ha221" "ɛ̆")
      ("ha222" "ń")
      ("ha223" "ý")
      ("ha224" "œ́")
      ("ha225" "Ý")
      ("ha226" "ǿ")
      ("ha227" "Ö")
      ("ha228" "Ü")
      ("ha229" "ß̈")
      ("ha22a" "Ä")
      ("ha22b" "Ú")
      ;; ?? unknown context
      ("ha22c" "|")
      ("ha22d" "ɐ̆")
      ;; ?? related to ha228 ??
      ("ha22e" "Ü")
      ;; caron or breve?
      ("ha22f" "ǒ")
      ("ha230" "ˇ")
      ;; Yuan sign is confused with Yen sign.
      ("ha231" "￥")

      ("za421" "→")
      ("za422" "⇔")
      ("za423" "†")
      ("za424" "‡")
      ("za425" "〔")
      ("za426" "〕")
      ("za427" "éi")
      ("za428" "≤")
      ("za429" "ʏ́e")
      ("za42a" "ui")
      ;; 0xa42b…
      ("za42b" "...")
      ("za42c" "<1>")
      ("za42d" "<2>")
      ("za42e" "<3>")
      ("za42f" "<4>")

      ("za430" "<5>")
      ("za431" "<6>")
      ("za432" "<7>")
      ("za433" "<8>")
      ("za434" "<9>")
      ("za435" "<10>")
      ("za436" "<11>")
      ("za437" "<12>")
      ("za438" "<13>")
      ("za439" "<14>")
      ("za43a" "<15>")
      ("za43b" "<16>")
      ("za43c" "<17>")
      ("za43d" "<18>")
      ("za43e" "<19>")
      ("za43f" "<20>")

      ("za440" "<21>")
      ("za441" "<22>")
      ("za442" "<23>")
      ("za443" "楣")
      ("za444" "煆")
      ("za445" "痤")
      ("za446" "袪")
      ("za447" "跗")
      ("za448" "瘭")
      ("za449" "簎")
      ("za44a" "骶")
      ;; 0xa44b ⇨
      ("za44b" "⇒")
      ("za44c" "〓(a44c)")
      ("za44d" "∴")
      ("za44e" " ̮")
      ("za44f" "©")

      ("za450" "Œ")
      ("za451" "ai")
      ("za452" "au")
      ("za453" "ɔy")
      ("za454" "ái")
      ("za455" "áu")
      ("za456" "ɔ́y")
      ("za457" "pf")
      ;; 0xa458 ʦ
      ("za458" "ts")
      ("za459" "tʃ")
      ("za45a" "dʒ")
      ("za45b" "⇀")
      ("za45c" "枘")
      ;; 新字体
      ("za45d" "癤")
      ("za45e" "〓(a45e)")
      ("za45f" "窠")
      ("za460" "ン")
      ("za461" "[1]")
      ("za462" "[2]")
      ("za463" "[3]")
      ("za464" "[4]")
      ("za465" "[5]")
      ("za466" "[6]")
      ("za467" "[7]")
      ("za468" "[8]")
      ("za469" "[9]")
      ("za46a" "[10]")
      ("za46b" "[11]")
      ("za46c" "[12]")
      ("za46d" "[13]")
      ("za46e" "[14]")
      ("za46f" "[15]")

      ("za470" "[16]")
      ("za471" "[17]")
      ("za472" "[18]")
      ("za473" "[19]")
      ("za474" "[20]")
      ("za475" "[21]")
      ("za476" "[22]")
      ("za477" "[23]")
      ("za478" "〓(a478)")
      ("za479" "─")
      ("za47a" "━")

      )))
  (defconst crown-gaiji-table
    (lookup-new-gaiji-table
     (mapcar 
      '(lambda (x)
        (list
         (if (eq lookup-support-agent 'ndtp)
             (concat "gaiji:" (car x))
           (car x))
         (cadr x)))
      encoded-crown-gaiji-table))))

(setq lookup-support-options
      (list ':gaiji-table crown-gaiji-table))

;;; support-crown.el ends here
