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
(require 'support-generic)

(let 
  ((encoded-crown-gaiji-table
    '(
      ("ha121" "&#x00C7;")
      ("ha122" "&#x0153;")
      ("ha123" "&#x00e7;")
      ("ha124" "&#x00e2;")
      ("ha125" "&#x00E3;")
      ("ha126" "&#x0216;")
      ("ha127" "&#x0259;")
      ("ha128" "&#x0254;")
      ("ha129" "&#x025b;")
      ("ha12a" "&#x0261;")
      ("ha12b" "&#x0272;")
      ("ha12c" "&#x014B;")
      ("ha12d" "&#x00F8;")
      ("ha12e" "&#x0283;")
      ("ha12f" "&#x0292;")
      ("ha130" "&#x02d0;")
      ("ha131" "&#x0251;&#x0300;")
      ("ha132" "&#x0251;&#x0303;")
      ("ha133" "&#x00E6;&#x0303;")
      ("ha134" "&#x0254;&#x0303;")
      ("ha135" "&#x025b;&#x0303;")
      ("ha136" "&#x0265;")
      ;; ??
      ("ha137" "&#x02cf;")
      ("ha138" "&#x012B;")
      ("ha139" "&#x00F1;")
      ("ha13a" "&#x00F5;")
      ("ha13b" "&#x0153;&#x0303;")
      ("ha13c" "&#x00C9;")
      ("ha13d" "&#x00E1;")
      ("ha13e" "&#x00E9;")
      ("ha13f" "&#x00B4;")
      ("ha140" "&#x00C0;")
      ("ha141" "&#x00C8;")
      ("ha142" "&#x00E0;")
      ("ha143" "&#x00E8;")
      ("ha144" "&#x00F9;")
      ("ha145" "&#x0060;")
      ("ha146" "&#x00E4;")
      ("ha147" "&#x00EB;")
      ("ha148" "&#x00F6;")
      ("ha149" "&#x00FC;")
      ("ha14a" "&#x00A8;")
      ("ha14b" "&#x00EF;")
      ("ha14c" "&#x00C2;")
      ("ha14d" "&#x00CA;")
      ("ha14e" "&#x00D4;")
      ("ha14f" "&#x00EA;")
      ("ha150" "&#x00EE;")
      ("ha151" "&#x00F4;")
      ("ha152" "&#x00FB;")
      ("ha153" "&#x005E;")
      ("ha154" "&#x00EE;")
      ("ha155" "&#x210a;")
      ("ha156" "&#x00E6;")
      ("ha157" "&#x0131;")
      ("ha158" "&#x03B2;")
      ("ha159" "&#x00ED;")
      ("ha15a" "&#x00F3;")
      ("ha15b" "&#x00FA;")
      ("ha15c" "&#x0103;")
      ;; caron or breve?
      ("ha15d" "&#x01D0;")
      ("ha15e" "&#x016D;")
      ("ha15f" "&#x0398;")
      ("ha160" "&#x028c;")
      ("ha161" "&#x00F2;")
      ("ha162" "&#x0251;&#x0301;")
      ("ha163" "&#x01fd;")
      ("ha164" "&#x0259;&#x0301;")
      ("ha165" "&#x0254;&#x0301;")
      ("ha166" "&#x025b;&#x0301;")
      ("ha167" "&#x029c;&#x0301;")
      ("ha168" "-&#x0301;")
      ("ha169" "&#x026A;")
      ;; small letter U, V.
      ("ha16a" "U")
      ("ha16b" "V")
      ("ha16c" "&#x028F;")
      ("ha16d" "&#x2113;")
      ;; delta sign?
      ("ha16e" "/")
      ;; ??
      ("ha16f" ";")
      ("ha170" "-")
      ;; ?
      ("ha171" "+")
      ("ha172" "&#x1e4d;")
      ("ha173" "&#x00c3;&#x0301;")
      ("ha174" "&#x025b;&#x0303;&#x0301;")
      ("ha175" "&#x0254;&#x0303;&#x0301;")
      ("ha176" "&#x0250;")
      ("ha177" "&#x0250;&#x030c;")
      ("ha178" "&#x00f1;")
      ("ha179" "&#x0250;&#x0303;")
      ("ha17a" "&#x010C;")
      ("ha17b" "&#x010D;")
      ("ha17c" "&#x017D;")
      ;; caron or breve??
      ("ha17d" "y&#x0306;")
      ;; caron or breve??
      ("ha17e" "&#x013E;")
      ("ha221" "&#x025b;&#x0306;")
      ("ha222" "&#x0144;")
      ("ha223" "&#x00FD;")
      ("ha224" "&#x0153;&#x0301;")
      ("ha225" "&#x00DD;")
      ("ha226" "&#x01FF;")
      ("ha227" "&#x00D6;")
      ("ha228" "&#x00DC;")
      ("ha229" "&#x00DF;&#x0308;")
      ("ha22a" "&#x00C4;")
      ("ha22b" "&#x00DA;")
      ;; ?? unknown context
      ("ha22c" "|")
      ("ha22d" "&#x0250;&#x0306;")
      ;; ?? related to ha228 ??
      ("ha22e" "&#x00DC;")
      ;; caron or breve?
      ("ha22f" "&#x01D2;")
      ("ha230" "&#x02C7;")
      ;; Yuan sign is confused with Yen sign.
      ("ha231" "￥")

      ("za421" "→")
      ("za422" "⇔")
      ("za423" "&#x2020;")
      ("za424" "&#x2021;")
      ("za425" "〔")
      ("za426" "〕")
      ("za427" "&#x00e9;i")
      ("za428" "&#x2264;")
      ("za429" "&#x028f;&#x0301;e")
      ("za42a" "ui")
      ;; 0xa42b&#x2026;
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
      ("za443" "&#x6963;")
      ("za444" "&#x7146;")
      ("za445" "&#x75e4;")
      ("za446" "&#x88aa;")
      ("za447" "&#x8dd7;")
      ("za448" "&#x762d;")
      ("za449" "&#x7c0e;")
      ("za44a" "&#x9ab6;")
      ;; 0xa44b &#x21e8;
      ("za44b" "⇒")
      ("za44c" "〓(a44c)")
      ("za44d" "∴")
      ("za44e" "&#x0020;&#x032e;")
      ("za44f" "&#x00a9;")

      ("za450" "&#x0152;")
      ("za451" "ai")
      ("za452" "au")
      ("za453" "&#x0254;y")
      ("za454" "&#x00e1;i")
      ("za455" "&#x00e1;u")
      ("za456" "&#x0254;&#x0301;y")
      ("za457" "pf")
      ;; 0xa458 &#x02A6;
      ("za458" "ts")
      ("za459" "t&#x0283;")
      ("za45a" "d&#x0292;")
      ("za45b" "&#x21c0;")
      ("za45c" "&#x6798;")
      ;; 新字体
      ("za45d" "&#x7664;")
      ("za45e" "〓(a45e)")
      ("za45f" "&#x7aa0;")
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
         (decode-character-string (cadr x))))
      encoded-crown-gaiji-table))))

(setq lookup-support-options
      (list ':gaiji-table crown-gaiji-table))

;;; support-crown.el ends here
