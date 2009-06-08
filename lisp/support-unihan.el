;;; support-unihan.el --- support file for "Unihan" file.
;; Copyright (C) 2009 Lookup Development Team

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

;;; Documentation:

;; This support-file will search the Unihan file distributed by
;; Unicode Consortium.  It also has an ability to refer Kangxi
;; Dictionary.
;;
;; You need to make the suffix array index by "mksary" program.  (-l
;; option should be attached, or some specific index point file should
;; be prepared, unless you want to search every contents.)
;;
;; Download site:
;; http://unicode.org/Public/5.0.0/ucd/Unihan.html

;;; Code:

(require 'lookup)

;;;
;;; Customizable Variable
;;;

(defvar support-unihan-kangxi-url-format
  "http://kangxizidian.com/kangxi/%04d.gif"
  "URL-Format of Scanned Image of KangXi Dictionary.
If you have a local copy of KangXi Dictionary, you should
customize this variable.")

(defvar support-unihan-information-format
  '(
    ("kJapaneseKun" "訓読み")
    ("kJapaneseOn" "音読み")
    ("kJis0" "JIS X 0208:1997")
    ("kJis1" "JIS X 0212:1990")
    ("kJIS0213" "JIS X 0213:2000")
    ("kRSAdobe_Japan1_6" "AJ16 CID & 部首画数")
    ("kIBMJapan" "IBM日本漢字コード")
    (nil "-----")
    ("kRSJapanese" "部首画数（日本用）")
    ("kRSKanWa" "部首画数（大漢和辞典）")
    ("kMorohashi" "大漢和辞典")
    ("kTotalStrokes" "総画数")
    ("kFourCornerCode" "四角号碼")
    ("kKangXi" "康煕字典" support-unihan-kangi-set-link)
    ("kRSKangXi" "部首画数（康煕字典）")
    (nil "-----")
    ("kSemanticVariant" "異体字")
    ("kCompatibilityVariant" "異体字（互換漢字）")
    ("kZVariant" "異体字（Z軸）")
    ("kSimplifiedVariant" "異体字（簡体字）")
    ("kTraditionalVariant" "異体字（繁体字）")
    ("kSpecializedSemanticVariant" "異体字（特殊）")
    ("kPrimaryNumeric" "漢数字")
    ("kAccountingNumeric" "漢数字（大字）")
    ("kOtherNumeric" "漢数字（他）")
    ("kDefinition" "字義")
    (nil "-----")
    ("kMandarin" "普通話")
    ("kCantonese" "廣東語")
    ("kTang" "唐代中古音")
    ("kGB0" "GB 2312")
    ("kGB1" "GB 12345")
    ("kGB3" "GB 13131")
    ("kGB5" "GB 13132")
    ("kGB7" "GB 8565-89")
    ("kGB8" "GB 8")
    (nil "-----")
    ("kBigFive" "Big5")
    ("kCNS1986" "CNS 11643:1986")
    ("kCNS1992" "CNS 11643:1992")
    ("kHKSCS" "香港 HKSCS")
    ("kCCCII" "CCCII")
    ("kEACC" "EACC (ANSI Z39.64)")
    (nil "-----")
    ("kKorean" "韓国語発音")
    ("kHangul" "ハングル")
    ("kKPS0" "KPS 9566-97")
    ("kKPS1" "KPS 10721-2000")
    ("kKSC0" "KS X 1001:1992")
    ("kKSC1" "KS X 1002:1991")
    ("kDaeJaweon" "大字源（韓国）")
    ("kRSKorean" "部首画数（韓国）")
    (nil "-----")
    ("kVietnamese" "ベトナム語")
    (nil "-----")
    ("kIRGKangXi" "康煕字典 (IRG)" support-unihan-kangi-set-link)
    ("kIRGDaiKanwaZiten" "大漢和辞典 (IRG)")
    ("kIRGDaeJaweon" "大字源 (IRG)")
    ("kIRGHanyuDaZidian" "漢語大字典 (IRG)")
    ("kIRG_GSource" "中国ソース (IRG)")
    ("kIRG_JSource" "日本ソース (IRG)")
    ("kIRG_KPSource" "北朝鮮ソース (IRG)")
    ("kIRG_KSource" "韓国ソース (IRG)")
    ("kIRG_TSource" "台湾ソース (IRG)")
    ("kIRG_VSource" "ベトナムソース (IRG)")
    ("kIRG_USource" "Unicodeソース (IRG)")
    (nil "-----")
    ("kRSUnicode" "部首画数（Unicode）")
    ("kIICore" "IICore")
    (nil "-----")
    ("kCihaiT" "辭海(1947年)")
    ("kHDZRadBreak" "漢語大字典")
    ("kHanYu" "漢語大詞典")
    ("kCangjie" "蒼頡コード")
    ("kSBGY" "宋音廣韻")
    ;; ("kCheungBauer" "Bauer's Cantonese Chars")
    ;; ("kCheungBauerIndex" "Bauer's Cantonese Chars Idx.")
    ;; ("kCowles" "Cowles 廣東語小辞典")
    ;; ("kFenn" "Fenn's Ch. Pocket Dict.")
    ;; ("kFennIndex" "Fenn's Ch. Pocket Dict., (1942)")
    ;; ("kFrequency" "中国語のUSENET投稿での頻度")
    ;; ("kGSR" "Grammata Serica Recensa (1957)")
    ;; ("kGradeLevel" "香港教育漢字")
    ;; ("kHKGlyph" "香港常用字字形表")
    ;; ("kHanyuPinlu" "現代漢語頻率詞典")
    ;; ("kKarlgren" "Analytic Dict. of Ch. and Sino-Ja.")
    ;; ("kLau" "Lau's Cantonese-Eng. Dict.")
    ;; ("kMainlandTelegraph" "中国漢字電報コード変換表")
    ;; ("kMatthews" "Mathews' Chinese-Eng. Dict.")
    ;; ("kMeyerWempe" "Student's Cantonese-Eng. Dict.")
    ;; ("kNelson" "Nelson's Jpn-Eng Dict.")
    ;; ("kPhonetic" "Casey's Char.s Analytic Dict.")
    ;; ("kTaiwanTelegraph" "台湾電報コード")
    ;; ("kXerox" "ゼロックスコード")
    )
  "Displayin Order and Header of Unihan DB.
First element of each list is key of Unihan, second is title, and
the third is a function to be processed (otherwise, Unicode in
the string will be converted to actual Character).  If first
element is nil, then second element will be displayed
unconditionally."  )

;;;
;;; Query-Filter
;;;

(defun support-unihan-query-filter (query)
  (setf (lookup-query-string query)
        (if (string-match "^[㐀-鿿𠀀-𯿼]$" (lookup-query-string query))
            (format "U+%X" (elt (lookup-query-string query) 0))
          (upcase (lookup-query-string query))))
  query)

;;;
;;; Search Tags
;;;

(defun support-unihan-head-tags (str)
  (string-match "\\(U\\+\\([0-9A-F]+\\)\\)" str)
  (concat (list (string-to-number (match-string 2 str) 16))
          "【" (match-string 1 str) "】"))

;;;
;;; arrangements
;;;

(defun support-unihan-unicode-to-char (str)
  (replace-regexp-in-string 
   "U\\+[0-9A-F]+"
   (lambda (x) (char-to-string (string-to-number (substring x 2) 16)))
   str))

(defun support-unihan-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (goto-char (point-min))
  (let* ((code (lookup-entry-code entry))
         (head (concat "【" (support-unihan-unicode-to-char code) "】"))
         (format support-unihan-information-format)
         entries)
    (while (re-search-forward "U\\+[0-9A-F]+	\\(.+\\)	\\(.+\\)" nil t)
      (setq entries (cons (cons (match-string 1) (match-string 2)) entries)))
    (setq entries
          (mapcar (lambda (x)
                    (let* ((key    (elt x 0))
                           (title  (elt x 1))
                           (proc   (elt x 2))
                           (entry  (assoc key entries))
                           (body   (cdr entry)))
                      (if (null key) (list title "")
                        (when body
                          (if (null proc) (setq proc 'support-unihan-unicode-to-char))
                          (setq body (apply proc (list body)))
                          (list title body)))))
                  format))
    (setq entries (remove-if 'null entries))
    (delete-region (point-min) (point-max))
    (insert head " " code "\n")
    (lookup-table-insert "%-10t %-20t\n" entries)))

(defun support-unihan-kangi-set-link (str)
  (replace-regexp-in-string 
   "[0-9][0-9][0-9][0-9]\\.[0-9][0-9]0"
   (lambda (x) 
     (lookup-url-set-link
      0 (1- (length x))
      (format support-unihan-kangxi-url-format
              (string-to-number (substring x 0 4)))
      x) x)
   str))

(setq lookup-support-options
      (list :title "Unihan"
            :arranges '((reference support-unihan-arrange-structure))
            :entry-tags '("" . "\t")
            :content-tags '("\n" . "\n")
            :code-tags  (lambda (x) 
                          (if (equal x 'search) '("\n" . "\t") '("" . "\t")))
            :head-tags  'support-unihan-head-tags
            :query-filter 'support-unihan-query-filter))

;;; support-unihan.el ends here
