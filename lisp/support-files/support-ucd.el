;;; ucd.el --- support file for "Unicode Character Database" file.
;; Copyright (C) 2013 Lookup Development Team

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

;; Download site:
;; http://unicode.org/Public/6.2.0/ucdxml/ucd.unihan.flat.zip

;;; Code:

;;;
;;; Customizable Variable
;;;

(defvar support-ucd-kangxi-url-format
  "http://kangxizidian.com/kangxi/%04d.gif"
  "URL-Format of Scanned Image of KangXi Dictionary.
If you have a local copy of KangXi Dictionary, you should
customize this variable.")

(defvar support-ucd-information-format
  '(
    ("cp" "符号位置")
    (nil "==== CJK Properties ====")
    ("kJapaneseKun" "訓読み")
    ("kJapaneseOn" "音読み")
    ("kJis0" "JIS X 0208:1997")
    ("kJis1" "JIS X 0212:1990")
    ("kJIS0213" "JIS X 0213:2000")
    ("kRSAdobe_Japan1_6" "AJ16 CID & 部首画数")
    ("kIBMJapan" "IBM日本漢字コード")
    ;;(nil "-----")
    ("kRSJapanese" "部首画数（日本用）")
    ("kRSKanWa" "部首画数（大漢和辞典）")
    ("kMorohashi" "大漢和辞典")
    ("kTotalStrokes" "総画数")
    ("kFourCornerCode" "四角号碼")
    ("kKangXi" "康煕字典" support-ucd-kangxi-set-link)
    ("kRSKangXi" "部首画数（康煕字典）")
    ;;(nil "-----")
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
    ;;(nil "-----")
    ("kMandarin" "普通話")
    ("kCantonese" "廣東語")
    ("kTang" "唐代中古音")
    ("kGB0" "GB 2312")
    ("kGB1" "GB 12345")
    ("kGB3" "GB 13131")
    ("kGB5" "GB 13132")
    ("kGB7" "GB 8565-89")
    ("kGB8" "GB 8")
    ("kHanyuPinyin" "漢語・ピンイン")
    ;;(nil "-----")
    ("kBigFive" "Big5")
    ("kCNS1986" "CNS 11643:1986")
    ("kCNS1992" "CNS 11643:1992")
    ("kHKSCS" "香港 HKSCS")
    ("kCCCII" "CCCII")
    ("kEACC" "EACC (ANSI Z39.64)")
    ;;(nil "-----")
    ("kKorean" "韓国語発音")
    ("kHangul" "ハングル")
    ("kKPS0" "KPS 9566-97")
    ("kKPS1" "KPS 10721-2000")
    ("kKSC0" "KS X 1001:1992")
    ("kKSC1" "KS X 1002:1991")
    ("kDaeJaweon" "大字源（韓国）")
    ("kRSKorean" "部首画数（韓国）")
    ;;(nil "-----")
    ("kVietnamese" "ベトナム語")
    ;;(nil "-----")
    ("kIRGKangXi" "康煕字典 (IRG)" support-ucd-kangxi-set-link)
    ("kIRGDaiKanwaZiten" "大漢和辞典 (IRG)")
    ("kIRGDaeJaweon" "大字源 (IRG)")
    ("kIRGHanyuDaZidian" "漢語大字典 (IRG)")
    ("kIRG_GSource" "中国ソース (IRG)")
    ("kIRG_JSource" "日本ソース (IRG)")
    ("kIRG_KPSource" "北朝鮮ソース (IRG)")
    ("kIRG_KSource" "韓国ソース (IRG)")
    ("kIRG_TSource" "台湾ソース (IRG)")
    ("kIRG_VSource" "ベトナムソース (IRG)")
    ("kIRG_HSource" "香港ソース (IRG)")
    ("kIRG_USource" "Unicodeソース (IRG)")
    ;;(nil "-----")
    ("kRSUnicode" "部首画数（Unicode）")
    ("kIICore" "IICore")
    ;;(nil "-----")
    ("kCihaiT" "辭海(1947年)")
    ("kHDZRadBreak" "漢語大字典")
    ("kHanYu" "漢語大詞典")
    ("kCangjie" "蒼頡コード")
    ("kSBGY" "宋音廣韻")
    ;;(nil "-----")
    ("kCheungBauer" "Bauer's Cantonese Chars")
    ("kCheungBauerIndex" "Bauer's Cantonese Chars Idx.")
    ("kCowles" "Cowles 廣東語小辞典")
    ("kFenn" "Fenn's Ch. Pocket Dict.")
    ("kFennIndex" "Fenn's Ch. Pocket Dict., (1942)")
    ("kFrequency" "中国語のUSENET投稿での頻度")
    ("kGSR" "Grammata Serica Recensa (1957)")
    ("kGradeLevel" "香港教育漢字")
    ("kHKGlyph" "香港常用字字形表")
    ("kHanyuPinlu" "現代漢語頻率詞典")
    ("kKarlgren" "Analytic Dict. of Ch. and Sino-Ja.")
    ("kLau" "Lau's Cantonese-Eng. Dict.")
    ("kMainlandTelegraph" "中国漢字電報コード変換表")
    ("kMatthews" "Mathews' Chinese-Eng. Dict.")
    ("kMeyerWempe" "Student's Cantonese-Eng. Dict.")
    ("kNelson" "Nelson's Jpn-Eng Dict.")
    ("kPhonetic" "Casey's Char.s Analytic Dict.")
    ("kTaiwanTelegraph" "台湾電報コード")
    ("kXerox" "ゼロックスコード")
    ("kXHC1983" "XHC1983")

    (nil "==== Numeric Properties ====")
    ("nv" "Numeric_Value")

    (nil "==== String Properties ====")
    ("cf" "Case_Folding")
    ("dm" "Decomposition_Mapping")
    ("FC_NFKC" "FC_NFKC_Closure")
    ("lc" "Lowercase_Mapping")
    ("NFKC_CF" "NFKC_Casefold")
    ("scf" "Simple_Case_Folding")
    ("slc" "Simple_Lowercase_Mapping")
    ("stc" "Simple_Titlecase_Mapping")
    ("suc" "Simple_Uppercase_Mapping")
    ("tc" "Titlecase_Mapping")
    ("uc" "Uppercase_Mapping")

    (nil "==== Miscellaneous Properties ====")
    ("bmg" "Bidi_Mirroring_Glyph")
    ("isc" "ISO_Comment")
    ("JSN" "Jamo_Short_Name")
    ("na" "Name")
    ("na1" "Unicode ver.1 Name")
    ("Name_Alias" "Name_Alias")
    ("scx" "Script_Extensions")

    (nil "==== Catalog Properties ====")
    ("age" "Age")
    ("blk" "Block")
    ("sc" "Script")

    (nil "==== Enumerated Properties ====")
    ("bc" "Bidi_Class")
    ("ccc" "Canonical_Combining_Class")
    ("dt" "Decomposition_Type")
    ("ea" "East_Asian_Width")
    ("gc" "General_Category")
    ("GCB" "Grapheme_Cluster_Break")
    ("hst" "Hangul_Syllable_Type")
    ("InMC" "Indic_Matra_Category")
    ("InSC" "Indic_Syllabic_Category")
    ("jg" "Joining_Group")
    ("jt" "Joining_Type")
    ("lb" "Line_Break")
    ("NFC_QC" "NFC_Quick_Check")
    ("NFD_QC" "NFD_Quick_Check")
    ("NFKC_QC" "NFKC_Quick_Check")
    ("NFKD_QC" "NFKD_Quick_Check")
    ("nt" "Numeric_Type")
    ("SB" "Sentence_Break")
    ("WB" "Word_Break")

    (nil "==== Binary Properties ====")
    ("AHex" "ASCII_Hex_Digit")
    ("Alpha" "Alphabetic")
    ("Bidi_C" "Bidi_Control")
    ("Bidi_M" "Bidi_Mirrored")
    ("Cased" "Cased")
    ("CE" "Composition_Exclusion")
    ("CI" "Case_Ignorable")
    ("Comp_Ex" "Full_Composition_Exclusion")
    ("CWCF" "Changes_When_Casefolded")
    ("CWCM" "Changes_When_Casemapped")
    ("CWKCF" "Changes_When_NFKC_Casefolded")
    ("CWL" "Changes_When_Lowercased")
    ("CWT" "Changes_When_Titlecased")
    ("CWU" "Changes_When_Uppercased")
    ("Dash" "Dash")
    ("Dep" "Deprecated")
    ("DI" "Default_Ignorable_Code_Point")
    ("Dia" "Diacritic")
    ("Ext" "Extender")
    ("Gr_Base" "Grapheme_Base")
    ("Gr_Ext" "Grapheme_Extend")
    ("Gr_Link" "Grapheme_Link")
    ("Hex" "Hex_Digit")
    ("Hyphen" "Hyphen")
    ("IDC" "ID_Continue")
    ("Ideo" "Ideographic")
    ("IDS" "ID_Start")
    ("IDSB" "IDS_Binary_Operator")
    ("IDST" "IDS_Trinary_Operator")
    ("Join_C" "Join_Control")
    ("LOE" "Logical_Order_Exception")
    ("Lower" "Lowercase")
    ("Math" "Math")
    ("NChar" "Noncharacter_Code_Point")
    ("OAlpha" "Other_Alphabetic")
    ("ODI" "Other_Default_Ignorable_Code_Point")
    ("OGr_Ext" "Other_Grapheme_Extend")
    ("OIDC" "Other_ID_Continue")
    ("OIDS" "Other_ID_Start")
    ("OLower" "Other_Lowercase")
    ("OMath" "Other_Math")
    ("OUpper" "Other_Uppercase")
    ("Pat_Syn" "Pattern_Syntax")
    ("Pat_WS" "Pattern_White_Space")
    ("QMark" "Quotation_Mark")
    ("Radical" "Radical")
    ("SD" "Soft_Dotted")
    ("STerm" "STerm")
    ("Term" "Terminal_Punctuation")
    ("UIdeo" "Unified_Ideograph")
    ("Upper" "Uppercase")
    ("VS" "Variation_Selector")
    ("WSpace" "White_Space")
    ("XIDC" "XID_Continue")
    ("XIDS" "XID_Start")
    ("XO_NFC" "Expands_On_NFC")
    ("XO_NFD" "Expands_On_NFD")
    ("XO_NFKC" "Expands_On_NFKC")
    ("XO_NFKD" "Expands_On_NFKD")
    )
  "Displayin Order and Header of UCD DB.
First element of each list is key of UCD, second is title, and
the third is a function to be processed (otherwise, Unicode in
the string will be converted to actual Character).  If first
element is nil, then second element will be displayed
unconditionally.
based on: http://www.unicode.org/Public/UNIDATA/PropertyAliases.txt"  )

;; Query-Filter
(defun support-ucd-query-filter (query)
  (lookup-new-query-filter
   query
   (lambda (str) 
     (format "%04X" (string-to-char str)))))

(defun support-ucd-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (goto-char (point-min))
  (let* ((code (lookup-entry-code entry))
         (head (concat "【"
                       (char-to-string (string-to-number code 16))
                        "】"))
         entries entrie2)
  (while (re-search-forward " \\([^= ]+?\\)=\"\\([^\"]+?\\)\"" nil t)
      (push (list (match-string 1) (match-string 2)) entries))
    (delete-region (point-min) (point-max))
    (insert head " " code "\n")
    (setq entries2 (nreverse (copy-sequence entries)))
    (loop for (key title proc) in support-ucd-information-format
          for body = (car (assoc-default key entries))
          if body
            collect (list title body) into results
            and do (lookup-assoc-del entries2 key)
          if (null key)
            collect (list title "") into results
          finally do
          (lookup-table-insert "%-10t %-20t\n" (append results entries2)))))

(defun support-ucd-kangxi-set-link (str)
  (replace-regexp-in-string 
   "[0-9][0-9][0-9][0-9]\\.[0-9][0-9]0"
   (lambda (x) 
     (lookup-set-link
      0 (1- (length x))
      (lookup-new-entry 
       'url nil ;; dict
       (format support-ucd-kangxi-url-format
               (string-to-number (substring x 0 4))))
      x) x)
   str))

(defun support-ucd-head-tags (content)
  (if (string-match "cp=\"\\([0-9A-F]+\\)\"" content)
      (format "【%c】%s" (string-to-number (match-string 1 content) 16)
              (match-string 1 content))
    (if (< 2 (length content))
        (error "UCD: Not proper XML content! %s" content))))

(setq lookup-support-options
      (list :title "Unicode Character Database"
            :query-filter 'support-ucd-query-filter
            :head-tags 'support-ucd-head-tags
            :content-tags '("\n" . "\n")
            :entry-tags '(" cp=\"" . "\"")
            :code-tags '(" cp=\"" . "\"")
            :charsets (lambda (x) (string-match "^.$" x))
            :arranges '((reference support-ucd-arrange-structure))))

;;; support-ucd.el ends here
