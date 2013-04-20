;;; lookup-text.el --- Lookup Text Utilities -*- lexical-binding: t -*-
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>
;; Copyright (C) 2013 Lookup Development Team <lookup@ring.gr.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Author: KAWABATA Taichi <kawabata.taichi@gmail.com>

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Documentation

;; This program collects various text utilities useful for lookup.  Notably:
;; * Word Picker
;; * Kanji-Kana Converter
;; * Hanzi-Pinyin Converter
;; * Katakana-Hiragana Converter
;; * Removal of Default Ignorable characters
;; * Old-Kanji - New-Kanji Converter
;; * Accent Removal
;; * Checks whether specified string belongs to specified coding-system.
;; * superscript/subscript text converter.

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'lookup-vars)
(require 'lookup-types)
(require 'stem-english)
;; (define-char-code-property 'hanzi-pinyin "hanzi-py-table.elc")

;;;
;;; Customizable Variables
;;;

(defcustom lookup-text-mecab-readings-option
  '("-N" "2" "-O" "yomi")
  "*漢字かな変換に指定するMecabへの引数。"
  :type 'list
  :group 'lookup-text)

;;;
;;; Internal Variables
;;;

(defvar lookup-text-reading-table (make-hash-table :test 'equal)
  "漢字ひらがな変換データのキャッシュ")

;;;
;;; Charsets Checker
;;;

(defun lookup-text-charsetsp (string charsets)
  "Determines if all of chars in STRING belongs to any of CHARSETS (or scripts).
If CHARSETS if function, then result of applying the function to
the string will be returned.  If CHARSETS is null, it returns t."
  (if (null charsets) t
    (if (functionp charsets) (funcall charsets string)
      (loop for char in (string-to-list string)
            always
            (loop for charset in charsets
                  thereis
                  (or (and (charsetp charset) (encode-char char charset))
                      (equal (aref char-script-table char) charset)))))))


;;;
;;; Word Pick-Up
;;;

(defvar lookup-word-category "w_")

(defun lookup-current-word ()
  (save-excursion
    (lookup-move-to-nearest-word)
    (let* ((word (lookup-current-word-constituent)))
      (cond ((lookup-text-charsetsp word '(ascii latin))
             (lookup-current-word-general))
            ((lookup-text-charsetsp word '(ascii 'japanese-jisx0208))
             (lookup-current-word-japanese))
            (t (lookup-current-word-general))))))

(defun lookup-move-to-nearest-word ()
  (unless (eq (char-syntax (or (char-after (point)) 0)) ?w)
    (skip-syntax-backward "^w" (line-beginning-position))
    (if (bolp)
        (skip-syntax-forward "^w" (line-end-position))
      (backward-char))))

(defun lookup-current-word-constituent ()
  (save-excursion
    (buffer-substring-no-properties
     (progn (skip-syntax-backward lookup-word-category) (point))
     (progn (skip-syntax-forward lookup-word-category) (point)))))

(defun lookup-current-word-general ()
  (thing-at-point 'word))

(defun lookup-current-word-japanese ()
  (save-excursion
    (cond 
     ((eq lookup-text-segmentize-japanese 'mecab)
      (let* ((start    (point))
             (text     (lookup-current-word-constituent))
             (segments (lookup-text-segmentize-by-mecab text))
             (regexp (concat "\\("
                             (replace-regexp-in-string
                              "[ \n]" "\\\\)\\\\(" segments)
                             "\\)"))
             (n 1))
        (skip-syntax-forward lookup-word-category)
        (if (null (re-search-backward regexp nil t))
            (progn (goto-char start) (thing-at-point 'word))
          (while (and (match-end n) (<= (match-end n) start))
            (incf n))
          (buffer-substring-no-properties (match-beginning n) (match-end n)))))
     (t (lookup-current-word-general)))))

;;;
;;; Text Segmentation
;;;

;;; Japanese text segmentation

(defvar lookup-mecab-wakati-command (list lookup-mecab-program "-O" "wakati"))

(defun lookup-text-segmentize-by-mecab (string)
  (lookup-with-coding-system lookup-mecab-coding-system
    (lookup-get-process-require lookup-mecab-wakati-command string)))

;;;
;;; String Conversion
;;;

;;; General function

(defun lookup-translate-string-by-table (str table)
  "Translate STR by TABLE (hash-table, char-table or char-code-property)."
  (let* ((chars (string-to-list str))
         (new-chars
          (typecase table
            (hash-table
             (mapcar (lambda (char)
                       (or (gethash char table) char))
                     chars))
            (char-table
             (mapcar (lambda (char)
                       (or (aref table char) char))
                     chars))
            (symbol
             (mapcar (lambda (char)
                       (or (get-char-code-property char table) char))
                     chars))))
         (new-chars-list (lookup-thread-list new-chars)))
    (mapcar (lambda (x) (apply 'lookup-concat x)) new-chars-list)))

(defun lookup-concat (&rest chars)
  "Like `concat', but can concatenate chars, lists, strings, and nil."
  (apply 'concat
         (mapcar (lambda (char)
                   (typecase char (null "") (character (list char))
                                  (t char)))
                   chars)))

;;; Kanji to Kana

(defun lookup-text-mecab-get-readings (str)
  (lookup-with-coding-system lookup-mecab-coding-system
    (with-temp-buffer
      (insert str)
      (apply 'call-process-region 1 (point-max) "mecab" t t nil 
             lookup-text-mecab-readings-option)
      (delete-dups
       (split-string (japanese-hiragana (buffer-string)))))))

(defun lookup-text-get-kana-readings (str)
  "Suppose STR is Kanji/Kana string and return its readings.
If it is not Kanji string, then it returns nil."
  (if (string-match "^\\(\\cH\\|\\cK\\|\\cC\\)+$" str)
      (let ((readings (gethash str lookup-text-reading-table)))
        (unless readings
          (setq readings (lookup-text-mecab-get-readings str))
          (puthash str readings lookup-text-reading-table))
        readings)))

;;; Hanzi to Pinyin

(defun lookup-text-hanzi-to-pinyin (str)
  "Convert Kanji STR to Pinyin."
  (require 'hanzi-py-table)
  (lookup-translate-string-by-table str 'hanzi-pinyin))

;;; Japanese Old Kanji to New Kanji and vice versa

(defvar lookup-old-to-new-kanji-table (make-hash-table))
(defvar lookup-new-to-old-kanji-jis-table (make-hash-table))
(defvar lookup-new-to-old-kanji-full-table (make-hash-table))
(defvar lookup-new-to-old-kanji-unicode-table (make-hash-table))
(mapc
 (lambda (x)
   (let ((new (car x)) (old (cdr x)))
     (lookup-add-to-table old new lookup-old-to-new-kanji-table)
     (when (equal (car (get-char-code-property old 'decomposition)) old)
       (lookup-add-to-table new old lookup-new-to-old-kanji-unicode-table))
     (when (encode-char old 'japanese-jisx0208)
       (lookup-add-to-table new old lookup-new-to-old-kanji-jis-table))
     (lookup-add-to-table new old lookup-new-to-old-kanji-full-table)))
  '((?万 . ?萬) (?与 . ?與) (?両 . ?兩) (?並 . ?竝) (?乗 . ?乘)
    (?乱 . ?亂) (?予 . ?豫) (?争 . ?爭) (?亜 . ?亞) (?仏 . ?佛)
    (?仮 . ?假) (?会 . ?會) (?伝 . ?傳) (?体 . ?體) (?余 . ?餘)
    (?併 . ?倂) (?価 . ?價) (?侮 . ?侮) (?倹 . ?儉) (?偽 . ?僞)
    (?僧 . ?僧) (?免 . ?免) (?児 . ?兒) (?党 . ?黨) (?円 . ?圓)
    (?写 . ?寫) (?処 . ?處) (?剣 . ?劍) (?剤 . ?劑) (?剰 . ?剩)
    (?励 . ?勵) (?労 . ?勞) (?効 . ?效) (?勅 . ?敕) (?勉 . ?勉)
    (?勤 . ?勤) (?勧 . ?勸) (?勲 . ?勳) (?区 . ?區) (?医 . ?醫)
    (?卑 . ?卑) (?単 . ?單) (?即 . ?卽) (?厳 . ?嚴) (?参 . ?參)
    (?双 . ?雙) (?収 . ?收) (?叙 . ?敍) (?台 . ?臺) (?号 . ?號)
    (?喝 . ?喝) (?営 . ?營) (?嘆 . ?嘆) (?嘱 . ?囑) (?器 . ?器)
    (?団 . ?團) (?囲 . ?圍) (?図 . ?圖) (?国 . ?國) (?圏 . ?圈)
    (?圧 . ?壓) (?堕 . ?墮) (?塀 . ?塀) (?塁 . ?壘) (?塚 . ?塚)
    (?塩 . ?鹽) (?増 . ?增) (?墨 . ?墨) (?壊 . ?壞) (?壌 . ?壤)
    (?壮 . ?壯) (?声 . ?聲) (?壱 . ?壹) (?売 . ?賣) (?変 . ?變)
    (?奥 . ?奧) (?奨 . ?奬) (?嬢 . ?孃) (?学 . ?學) (?宝 . ?寶)
    (?実 . ?實) (?寛 . ?寬) (?寝 . ?寢) (?対 . ?對) (?寿 . ?壽)
    (?専 . ?專) (?将 . ?將) (?尽 . ?盡) (?届 . ?屆) (?属 . ?屬)
    (?層 . ?層) (?岳 . ?嶽) (?峡 . ?峽) (?巣 . ?巢) (?巻 . ?卷)
    (?帯 . ?帶) (?帰 . ?歸) (?庁 . ?廳) (?広 . ?廣) (?廃 . ?廢)
    (?廊 . ?廊) (?弁 . ?弁) (?弁 . ?瓣) (?弁 . ?辯) (?弁 . ?辨)
    (?弐 . ?貳) (?弾 . ?彈) (?当 . ?當) (?径 . ?徑) (?従 . ?從)
    (?徳 . ?德) (?徴 . ?徵) (?応 . ?應) (?恋 . ?戀) (?恒 . ?恆)
    (?恵 . ?惠) (?悔 . ?悔) (?悩 . ?惱) (?悪 . ?惡) (?惨 . ?慘)
    (?慎 . ?愼) (?慨 . ?慨) (?憎 . ?憎) (?懐 . ?懷) (?懲 . ?懲)
    (?戦 . ?戰) (?戯 . ?戲) (?戻 . ?戾) (?払 . ?拂) (?抜 . ?拔)
    (?択 . ?擇) (?担 . ?擔) (?拝 . ?拜) (?拠 . ?據) (?拡 . ?擴)
    (?挙 . ?擧) (?挟 . ?挾) (?挿 . ?插) (?捜 . ?搜) (?掲 . ?揭)
    (?揺 . ?搖) (?摂 . ?攝) (?撃 . ?擊) (?敏 . ?敏) (?数 . ?數)
    (?斉 . ?齊) (?斎 . ?齋) (?断 . ?斷) (?既 . ?既) (?旧 . ?舊)
    (?昼 . ?晝) (?晩 . ?晚) (?暁 . ?曉) (?暑 . ?暑) (?暦 . ?曆)
    (?朗 . ?朗) (?条 . ?條) (?来 . ?來) (?枢 . ?樞) (?栄 . ?榮)
    (?桜 . ?櫻) (?桟 . ?棧) (?梅 . ?梅) (?検 . ?檢) (?楼 . ?樓)
    (?楽 . ?樂) (?概 . ?槪) (?様 . ?樣) (?権 . ?權) (?横 . ?橫)
    (?欄 . ?欄) (?欠 . ?缺) (?欧 . ?歐) (?歓 . ?歡) (?歩 . ?步)
    (?歯 . ?齒) (?歴 . ?歷) (?残 . ?殘) (?殴 . ?毆) (?殺 . ?殺)
    (?殻 . ?殼) (?毎 . ?每) (?気 . ?氣) (?沢 . ?澤) (?浄 . ?淨)
    (?浅 . ?淺) (?浜 . ?濱) (?海 . ?海) (?涙 . ?淚) (?渇 . ?渴)
    (?済 . ?濟) (?渉 . ?涉) (?渋 . ?澁) (?渓 . ?溪) (?温 . ?溫)
    (?湾 . ?灣) (?湿 . ?濕) (?満 . ?滿) (?滝 . ?瀧) (?滞 . ?滯)
    (?漢 . ?漢) (?潜 . ?潛) (?瀬 . ?瀨) (?灯 . ?燈) (?炉 . ?爐)
    (?点 . ?點) (?為 . ?爲) (?焼 . ?燒) (?煮 . ?煮) (?犠 . ?犧)
    (?状 . ?狀) (?独 . ?獨) (?狭 . ?狹) (?猟 . ?獵) (?献 . ?獻)
    (?獣 . ?獸) (?瓶 . ?甁) (?画 . ?畫) (?畳 . ?疊) (?痴 . ?癡)
    (?発 . ?發) (?盗 . ?盜) (?県 . ?縣) (?真 . ?眞) (?研 . ?硏)
    (?砕 . ?碎) (?碑 . ?碑) (?礼 . ?禮) (?社 . ?社) (?祈 . ?祈)
    (?祉 . ?祉) (?祖 . ?祖) (?祝 . ?祝) (?神 . ?神) (?祥 . ?祥)
    (?禅 . ?禪) (?禍 . ?禍) (?福 . ?福) (?秘 . ?祕) (?称 . ?稱)
    (?稲 . ?稻) (?穀 . ?穀) (?穂 . ?穗) (?穏 . ?穩) (?突 . ?突)
    (?窃 . ?竊) (?竜 . ?龍) (?節 . ?節) (?粋 . ?粹) (?粛 . ?肅)
    (?糸 . ?絲) (?経 . ?經) (?絵 . ?繪) (?継 . ?繼) (?続 . ?續)
    (?総 . ?總) (?緑 . ?綠) (?緒 . ?緖) (?練 . ?練) (?縁 . ?緣)
    (?縄 . ?繩) (?縦 . ?縱) (?繁 . ?繁) (?繊 . ?纖) (?缶 . ?罐)
    (?署 . ?署) (?翻 . ?飜) (?者 . ?者) (?聴 . ?聽) (?胆 . ?膽)
    (?脳 . ?腦) (?臓 . ?臟) (?臭 . ?臭) (?芸 . ?藝) (?茎 . ?莖)
    (?荘 . ?莊) (?著 . ?著) (?蔵 . ?藏) (?薫 . ?薰) (?薬 . ?藥)
    (?虚 . ?虛) (?虜 . ?虜) (?虫 . ?蟲) (?蚕 . ?蠶) (?蛍 . ?螢)
    (?蛮 . ?蠻) (?衛 . ?衞) (?装 . ?裝) (?褐 . ?褐) (?褒 . ?襃)
    (?覇 . ?霸) (?視 . ?視) (?覚 . ?覺) (?覧 . ?覽) (?観 . ?觀)
    (?触 . ?觸) (?訳 . ?譯) (?証 . ?證) (?誉 . ?譽) (?読 . ?讀)
    (?諸 . ?諸) (?謁 . ?謁) (?謡 . ?謠) (?謹 . ?謹) (?譲 . ?讓)
    (?豊 . ?豐) (?賓 . ?賓) (?賛 . ?贊) (?贈 . ?贈) (?践 . ?踐)
    (?転 . ?轉) (?軽 . ?輕) (?辞 . ?辭) (?辺 . ?邊) (?逓 . ?遞)
    (?逸 . ?逸) (?遅 . ?遲) (?郎 . ?郞) (?郷 . ?鄕) (?都 . ?都)
    (?酔 . ?醉) (?醸 . ?釀) (?釈 . ?釋) (?鉄 . ?鐵) (?鉱 . ?鑛)
    (?銭 . ?錢) (?鋳 . ?鑄) (?錬 . ?鍊) (?録 . ?錄) (?鎮 . ?鎭)
    (?関 . ?關) (?闘 . ?鬪) (?陥 . ?陷) (?険 . ?險) (?隆 . ?隆)
    (?随 . ?隨) (?隠 . ?隱) (?雑 . ?雜) (?難 . ?難) (?霊 . ?靈)
    (?静 . ?靜) (?響 . ?響) (?頻 . ?頻) (?頼 . ?賴) (?顕 . ?顯)
    (?類 . ?類) (?駅 . ?驛) (?駆 . ?驅) (?騒 . ?騷) (?験 . ?驗)
    (?髄 . ?髓) (?髪 . ?髮) (?鶏 . ?鷄) (?麦 . ?麥) (?黄 . ?黃)
    (?黒 . ?黑) (?黙 . ?默) (?齢 . ?齡)))

(defun lookup-text-old-to-new-kanji (str)
  (car (lookup-translate-string-by-table str lookup-old-to-new-kanji-table)))

(defun lookup-text-new-to-old-jis-kanji (str)
  ;; e.g. (lookup-new-to-old-jis-kanji "置弁") -> ("置辨" "置辯" ...)
  (lookup-translate-string-by-table str lookup-new-to-old-kanji-jis-table))

(defun lookup-text-new-to-old-unicode-kanji (str)
  (lookup-translate-string-by-table str lookup-new-to-old-kanji-unicode-table))

;;; Remove accents

(defun lookup-remove-accents (string)
  (let* ((nfd (ucs-normalize-NFD-string string))
         (chars (string-to-list nfd)))
    (apply 'string
           (cl-remove-if
            (lambda (x)
              (let ((ccc 
                     (get-char-code-property x 'canonical-combining-class)))
                (and ccc (< 0 ccc)))) chars))))

;;; Superscript/Subscript

(defvar math-symbols-superscript-table)
(defvar math-symbols-subscript-table)

(defun lookup-superscript-string (str)
  (require 'math-symbols nil t)
  (apply 'lookup-concat
         (mapcar (lambda (char)
                   (or (and (hash-table-p math-symbols-superscript-table)
                            (gethash char math-symbols-superscript-table))
                       (propertize (string char) 'display '((raise 0.3) (height 0.8)))))
                 (string-to-list str))))

(defun lookup-subscript-string (str)
  (require 'math-symbols nil t)
  (apply 'lookup-concat
         (mapcar (lambda (char)
                   (or (and (hash-table-p math-symbols-subscript-table)
                            (gethash char math-symbols-subscript-table))
                       (propertize (string char) 'display '((raise -0.3) (height 0.8)))))
                 (string-to-list str))))

;;;
;;; query-filter
;;;

(defmacro lookup-new-query-filter (query function)
  "Create a new query filter with string-convert FUNCTION applied.
FUNCTION may return multiple results."
  `(let* ((string (lookup-query-string ,query))
          (method (lookup-query-method ,query))
          (results (funcall ,function string)))
     (if (not (listp results)) (setq results (list results)))
     (mapcar (lambda (result)
               (lookup-new-query method result)) results)))

;;; kanji/hanzi filters

(defun lookup-query-filter-old-to-new-kanji (query)
  (lookup-new-query-filter query 'lookup-text-old-to-new-kanji))

(defun lookup-query-filter-hanzi-to-pinyin (query)
  (lookup-new-query-filter query 'lookup-text-hanzi-to-pinyin))

;;; accent removal filter

(defun lookup-query-filter-remove-accents (query)
  (lookup-new-query-filter query 'lookup-remove-accents))

;;; normalization filter

(defun lookup-query-filter-decode-url (query)
  (lookup-new-query-filter query 'url-unhex-string))

(defun lookup-query-filter-normalize-nfc (query)
  (lookup-new-query-filter query 'ucs-normalize-NFC-string))

(defun lookup-query-filter-normalize-nfkc (query)
  (lookup-new-query-filter query 'ucs-normalize-NFKC-string))

(defun lookup-remove-default-ignorables (string)
  "remove Default Ignorable characters."
  (replace-regexp-in-string "[〾󠀀-󯿽]" "" string))

(defun lookup-query-filter-remove-default-ignorables (query)
  (lookup-new-query-filter query 'lookup-remove-default-ignorables))

;;; downcase filter

(defun lookup-query-filter-downcase (query)
  (lookup-new-query-filter query 'downcase))

(add-to-list 'lookup-query-filters 'lookup-query-filter-downcase)

;;; English stemming

(defun lookup-query-filter-stem-english (query)
  (require 'stem-english)
  (let* ((string (downcase (lookup-query-string query)))
         (method (lookup-query-method query)))
    (if (or (equal method 'exact) (equal method 'keyword))
        (mapcar 
         (lambda (x) (lookup-new-query method x))
         (cons string
               (cl-remove-if
                (lambda (x) (< (length x) 4))
                (cdr (nreverse (stem-english string))))))
      (list query))))

;;; misc. filters

(defun lookup-query-filter-kanji-to-kana (query)
  (mapcar (lambda (x) (lookup-new-query (lookup-query-method query) x))
          (lookup-text-get-kana-readings (lookup-query-string query))))

(defun lookup-query-filter-to-katakana (query)
  (lookup-new-query-filter query 'japanese-katakana))

;;; default filter settings
(add-to-list 'lookup-query-filters 'lookup-query-filter-normalize-nfkc)
(add-to-list 'lookup-query-filters 'lookup-query-filter-decode-url)
(add-to-list 'lookup-query-filters 'lookup-query-filter-remove-default-ignorables)


(provide 'lookup-text)

;;; lookup-text.el ends here
