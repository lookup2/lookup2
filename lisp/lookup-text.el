;;; lookup-text.el --- Lookup Text Utilities
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>
;; Copyright (C) 2009 Lookup Development Team <lookup@ring.gr.jp>

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

;; This program collects various text utilities useful for 
;; dictionary lookup.  Notably:
;; * Kanji-Kana Converter
;; * Kanji-Pinyin Converter
;; * Old-Kanji - New-Kanji Converter
;; * Accent Removal
;; * Checks whether specified string belongs to specified coding-system.
;; * superscript/subscript text converter.

;;; Code:

(require 'lookup-vars)
(require 'ucs-normalize)

;;;
;;; Customizable Variables
;;;

(defcustom lookup-text-mecab-readings-option
  '("-N" "2" "-O" "yomi")
  "*漢字かな変換に指定するMecabへの引数。"
  :type 'list
  :group 'lookup-text)

(defcustom lookup-text-pinyin-file (expand-file-name "~/cvs/emacs/leim/MISC-DIC/pinyin.map")
  "*漢字ピンイン変換に使用するデータがあるファイル。
Emacs配布の`emacs/leim/MISC-DIC/pinyin.map'を指定する。"
  :type 'string
  :group 'lookup-text)

;;;
;;; Internal Variables
;;;

(defvar lookup-text-reading-hash (make-hash-table :test 'equal)
  "漢字ひらがな変換データのキャッシュ")

(defvar lookup-text-pinyin-table nil
  "漢字ピンイン変換用ハッシュテーブル")

;;;
;;; Wakati Gaki
;;;

(defvar lookup-mecab-wakati-command (list lookup-mecab-program "-O" "wakati"))

;;;###autoload
(defun lookup-text-wakati (string)
  (lookup-with-coding-system lookup-mecab-coding-system
    (lookup-get-process-require lookup-mecab-wakati-command string)))

;;;
;;; Kanji to Kana 
;;;

;; Currently, we do not use the above interface for get-readings, 
;; because mecab `--nbest' option do not provide the way to detect ending.

(defun lookup-text-mecab-get-readings (str)
  (lookup-with-coding-system lookup-mecab-coding-system
    (with-temp-buffer
      (let ((process
             (apply 
              'start-process 
              `("mecab-reading" ,(current-buffer)
                ,lookup-mecab-program
                ,@lookup-text-mecab-readings-option))))
        (process-send-string process (concat str "\n"))
        (sit-for 0.1)
        (remove-duplicates 
         (split-string (japanese-hiragana (buffer-string)))
         :test 'equal)))))

;;;###autoload
(defun lookup-text-get-readings (str)
  "STR を漢字ひらがな変換して得られた結果のリストを返す関数.
変換できない文字が含まれていた場合は、nilを返す。"
  (if (and (lookup-text-charsetsp str '(japanese-jisx0208))
           (string-match "^[あ-んア-ンー一-鿿]+$" str))
      (let ((readings (gethash str lookup-text-reading-hash)))
        (unless readings
          (setq readings (lookup-text-mecab-get-readings str))
          (puthash str readings lookup-text-reading-hash))
        readings)))

;;(defun lookup-text-filter (str entries)
;;  "STR が見出しに含まれているようなエントリのみを取り出すフィルタ関数"
;;  (let ((regex (mapconcat 
;;                'char-to-string (string-to-char-list string) ".?.?.?")))
;;    (delq nil (mapcar 
;;               (lambda (entry)
;;                 (if (string-match regex (lookup-entry-heading entry)) entry))
;;               entries))))

;;;
;;; Kanji to Pinyin
;;;

;;;###autoload
(defun lookup-text-get-pinyin (str)
  "Convert Kanji STR to Pinyin.
If ANY kanji failed to be converted, then nil will be returned."
  (when (file-exists-p lookup-text-pinyin-file)
    (unless lookup-text-pinyin-table
      (setq lookup-text-pinyin-table (make-hash-table :test 'equal))
      (with-temp-buffer
        (let ((coding-system-for-read 'euc-china)
              pinyin)
          (insert-file-contents lookup-text-pinyin-file)
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-z]+\\)	\\(.+\\)" nil t)
            (setq pinyin (match-string 1))
            (dolist (char (string-to-list (match-string 2)))
              (puthash char pinyin lookup-text-pinyin-table))))))
    (let* ((chars (string-to-list str))
           (pys (mapcar (lambda (x) (gethash x lookup-text-pinyin-table))
                        chars)))
      (unless (memq nil pys) (apply 'concat pys)))))

;;;
;;; Charsetsp Function
;;;

(defun lookup-text-charsetsp (string charsets)
  "Determines if all of chars in STRING belongs to any of CHARSETS list.
If CHARSETS if function, then result of applying the function to
the string will be returned.  If CHARSETS is null, it returns t."
  (if (null charsets) t
    (if (functionp charsets) (funcall charsets string)
      (let ((flag t) (chars (string-to-list string)) charsets-2)
        (while (and flag chars)
          (setq charsets-2 charsets flag nil)
          (while (and (null flag) charsets-2)
            (if (encode-char (car chars) (car charsets-2))
                (setq flag t)
              (setq charsets-2 (cdr charsets-2))))
          (setq chars (cdr chars)))
        flag))))

(defun lookup-text-cjk-p (string)
  "Determines if STRING consists of CJK Unified Ideogrphs."
  (if (string-match "^[㐀-鿿𠀀-𯟿]+$" string) t))

(defun lookup-text-single-cjk-p (string)
  "Determines if STRING is one single CJK Unified Ideogrph."
  (if (string-match "^[㐀-鿿𠀀-𯟿]$" string) t))

;;
;; Normalize Input String
;;

(defun lookup-normalize-query-string ()
  (setq lookup-query-string
        (replace-regexp-in-string
         "[〾󠀀-󯿽]" ""
        (ucs-normalize-NFC-string lookup-query-string))))

(add-hook 'lookup-query-string-hook 'lookup-normalize-query-string)

;;
;; 日本の旧字・新字の対応
;;

(defvar lookup-text-old-new-alist
  '((?万 . ?萬) (?与 . ?與) (?両 . ?兩) (?並 . ?竝)
    (?乗 . ?乘) (?乱 . ?亂) (?予 . ?豫) (?争 . ?爭)
    (?亜 . ?亞) (?仏 . ?佛) (?仮 . ?假) (?会 . ?會)
    (?伝 . ?傳) (?体 . ?體) (?余 . ?餘) (?併 . ?倂)
    (?価 . ?價) (?侮 . ?侮) (?倹 . ?儉) (?偽 . ?僞)
    (?僧 . ?僧) (?免 . ?免) (?児 . ?兒) (?党 . ?黨)
    (?円 . ?圓) (?写 . ?寫) (?処 . ?處) (?剣 . ?劍)
    (?剤 . ?劑) (?剰 . ?剩) (?励 . ?勵) (?労 . ?勞)
    (?効 . ?效) (?勅 . ?敕) (?勉 . ?勉) (?勤 . ?勤)
    (?勧 . ?勸) (?勲 . ?勳) (?区 . ?區) (?医 . ?醫)
    (?卑 . ?卑) (?単 . ?單) (?即 . ?卽) (?厳 . ?嚴)
    (?参 . ?參) (?双 . ?雙) (?収 . ?收) (?叙 . ?敍)
    (?台 . ?臺) (?号 . ?號) (?喝 . ?喝) (?営 . ?營)
    (?嘆 . ?嘆) (?嘱 . ?囑) (?器 . ?器) (?団 . ?團)
    (?囲 . ?圍) (?図 . ?圖) (?国 . ?國) (?圏 . ?圈)
    (?圧 . ?壓) (?堕 . ?墮) (?塀 . ?塀) (?塁 . ?壘)
    (?塚 . ?塚) (?塩 . ?鹽) (?増 . ?增) (?墨 . ?墨)
    (?壊 . ?壞) (?壌 . ?壤) (?壮 . ?壯) (?声 . ?聲)
    (?壱 . ?壹) (?売 . ?賣) (?変 . ?變) (?奥 . ?奧)
    (?奨 . ?奬) (?嬢 . ?孃) (?学 . ?學) (?宝 . ?寶)
    (?実 . ?實) (?寛 . ?寬) (?寝 . ?寢) (?対 . ?對)
    (?寿 . ?壽) (?専 . ?專) (?将 . ?將) (?尽 . ?盡)
    (?届 . ?屆) (?属 . ?屬) (?層 . ?層) (?岳 . ?嶽)
    (?峡 . ?峽) (?巣 . ?巢) (?巻 . ?卷) (?帯 . ?帶)
    (?帰 . ?歸) (?庁 . ?廳) (?広 . ?廣) (?廃 . ?廢)
    (?廊 . ?廊) (?弁 . ?瓣) (?弁 . ?辯) (?弁 . ?辨)
    (?弐 . ?貳) (?弾 . ?彈) (?当 . ?當) (?径 . ?徑)
    (?従 . ?從) (?徳 . ?德) (?徴 . ?徵) (?応 . ?應)
    (?恋 . ?戀) (?恒 . ?恆) (?恵 . ?惠) (?悔 . ?悔)
    (?悩 . ?惱) (?悪 . ?惡) (?惨 . ?慘) (?慎 . ?愼)
    (?慨 . ?慨) (?憎 . ?憎) (?懐 . ?懷) (?懲 . ?懲)
    (?戦 . ?戰) (?戯 . ?戲) (?戻 . ?戾) (?払 . ?拂)
    (?抜 . ?拔) (?択 . ?擇) (?担 . ?擔) (?拝 . ?拜)
    (?拠 . ?據) (?拡 . ?擴) (?挙 . ?擧) (?挟 . ?挾)
    (?挿 . ?插) (?捜 . ?搜) (?掲 . ?揭) (?揺 . ?搖)
    (?摂 . ?攝) (?撃 . ?擊) (?敏 . ?敏) (?数 . ?數)
    (?斉 . ?齊) (?斎 . ?齋) (?断 . ?斷) (?既 . ?既)
    (?旧 . ?舊) (?昼 . ?晝) (?晩 . ?晚) (?暁 . ?曉)
    (?暑 . ?暑) (?暦 . ?曆) (?朗 . ?朗) (?条 . ?條)
    (?来 . ?來) (?枢 . ?樞) (?栄 . ?榮) (?桜 . ?櫻)
    (?桟 . ?棧) (?梅 . ?梅) (?検 . ?檢) (?楼 . ?樓)
    (?楽 . ?樂) (?概 . ?槪) (?様 . ?樣) (?権 . ?權)
    (?横 . ?橫) (?欄 . ?欄) (?欠 . ?缺) (?欧 . ?歐)
    (?歓 . ?歡) (?歩 . ?步) (?歯 . ?齒) (?歴 . ?歷)
    (?残 . ?殘) (?殴 . ?毆) (?殺 . ?殺) (?殻 . ?殼)
    (?毎 . ?每) (?気 . ?氣) (?沢 . ?澤) (?浄 . ?淨)
    (?浅 . ?淺) (?浜 . ?濱) (?海 . ?海) (?涙 . ?淚)
    (?渇 . ?渴) (?済 . ?濟) (?渉 . ?涉) (?渋 . ?澁)
    (?渓 . ?溪) (?温 . ?溫) (?湾 . ?灣) (?湿 . ?濕)
    (?満 . ?滿) (?滝 . ?瀧) (?滞 . ?滯) (?漢 . ?漢)
    (?潜 . ?潛) (?瀬 . ?瀨) (?灯 . ?燈) (?炉 . ?爐)
    (?点 . ?點) (?為 . ?爲) (?焼 . ?燒) (?煮 . ?煮)
    (?犠 . ?犧) (?状 . ?狀) (?独 . ?獨) (?狭 . ?狹)
    (?猟 . ?獵) (?献 . ?獻) (?獣 . ?獸) (?瓶 . ?甁)
    (?画 . ?畫) (?畳 . ?疊) (?痴 . ?癡) (?発 . ?發)
    (?盗 . ?盜) (?県 . ?縣) (?真 . ?眞) (?研 . ?硏)
    (?砕 . ?碎) (?碑 . ?碑) (?礼 . ?禮) (?社 . ?社)
    (?祈 . ?祈) (?祉 . ?祉) (?祖 . ?祖) (?祝 . ?祝)
    (?神 . ?神) (?祥 . ?祥) (?禅 . ?禪) (?禍 . ?禍)
    (?福 . ?福) (?秘 . ?祕) (?称 . ?稱) (?稲 . ?稻)
    (?穀 . ?穀) (?穂 . ?穗) (?穏 . ?穩) (?突 . ?突)
    (?窃 . ?竊) (?竜 . ?龍) (?節 . ?節) (?粋 . ?粹)
    (?粛 . ?肅) (?糸 . ?絲) (?経 . ?經) (?絵 . ?繪)
    (?継 . ?繼) (?続 . ?續) (?総 . ?總) (?緑 . ?綠)
    (?緒 . ?緖) (?練 . ?練) (?縁 . ?緣) (?縄 . ?繩)
    (?縦 . ?縱) (?繁 . ?繁) (?繊 . ?纖) (?缶 . ?罐)
    (?署 . ?署) (?翻 . ?飜) (?者 . ?者) (?聴 . ?聽)
    (?胆 . ?膽) (?脳 . ?腦) (?臓 . ?臟) (?臭 . ?臭)
    (?芸 . ?藝) (?茎 . ?莖) (?荘 . ?莊) (?著 . ?著)
    (?蔵 . ?藏) (?薫 . ?薰) (?薬 . ?藥) (?虚 . ?虛)
    (?虜 . ?虜) (?虫 . ?蟲) (?蚕 . ?蠶) (?蛍 . ?螢)
    (?蛮 . ?蠻) (?衛 . ?衞) (?装 . ?裝) (?褐 . ?褐)
    (?褒 . ?襃) (?覇 . ?霸) (?視 . ?視) (?覚 . ?覺)
    (?覧 . ?覽) (?観 . ?觀) (?触 . ?觸) (?訳 . ?譯)
    (?証 . ?證) (?誉 . ?譽) (?読 . ?讀) (?諸 . ?諸)
    (?謁 . ?謁) (?謡 . ?謠) (?謹 . ?謹) (?譲 . ?讓)
    (?豊 . ?豐) (?賓 . ?賓) (?賛 . ?贊) (?贈 . ?贈)
    (?践 . ?踐) (?転 . ?轉) (?軽 . ?輕) (?辞 . ?辭)
    (?辺 . ?邊) (?逓 . ?遞) (?逸 . ?逸) (?遅 . ?遲)
    (?郎 . ?郞) (?郷 . ?鄕) (?都 . ?都) (?酔 . ?醉)
    (?醸 . ?釀) (?釈 . ?釋) (?鉄 . ?鐵) (?鉱 . ?鑛)
    (?銭 . ?錢) (?鋳 . ?鑄) (?錬 . ?鍊) (?録 . ?錄)
    (?鎮 . ?鎭) (?関 . ?關) (?闘 . ?鬪) (?陥 . ?陷)
    (?険 . ?險) (?隆 . ?隆) (?随 . ?隨) (?隠 . ?隱)
    (?雑 . ?雜) (?難 . ?難) (?霊 . ?靈) (?静 . ?靜)
    (?響 . ?響) (?頻 . ?頻) (?頼 . ?賴) (?顕 . ?顯)
    (?類 . ?類) (?駅 . ?驛) (?駆 . ?驅) (?騒 . ?騷)
    (?験 . ?驗) (?髄 . ?髓) (?髪 . ?髮) (?鶏 . ?鷄)
    (?麦 . ?麥) (?黄 . ?黃) (?黒 . ?黑) (?黙 . ?默)
    (?齢 . ?齡)))

(defun lookup-text-old-to-new (str)
  "Convert old Kanji in STR to new Kanji."
  (let* ((chars (string-to-list str))
         ;; remove IVS characters
         (chars (remove-if (lambda (x) (and (< #xe0000 x)
                                            (< x #xeffff)))
                           chars))
         (chars (mapcar (lambda (x)
                          (or (car (rassoc x lookup-text-old-new-alist)) x))
                        chars)))
    (apply 'string chars)))

(defun lookup-text-old-to-new-query-string ()
  (setq lookup-query-string
        (lookup-text-old-to-new lookup-query-string)))

(add-hook 'lookup-query-string-hook 'lookup-text-old-to-new-query-string)

;;; Lookup superscript/subscript utilities

(defconst lookup-superscript-char-table
  '((?2 . ?²) (?3 . ?³) (?1 . ?¹)
    (?o . ?º) (?h . ?ʰ) (?ɦ . ?ʱ) (?j . ?ʲ)
    (?r . ?ʳ) (?ɹ . ?ʴ) (?ɻ . ?ʵ) (?ʁ . ?ʶ)
    (?w . ?ʷ) (?y . ?ʸ) (?ɣ . ?ˠ) (?l . ?ˡ)
    (?s . ?ˢ) (?x . ?ˣ) (?ʕ . ?ˤ) (?ნ . ?ჼ)
    (?A . ?ᴬ) (?Æ . ?ᴭ) (?B . ?ᴮ) (?D . ?ᴰ)
    (?E . ?ᴱ) (?Ǝ . ?ᴲ) (?G . ?ᴳ) (?H . ?ᴴ)
    (?I . ?ᴵ) (?J . ?ᴶ) (?K . ?ᴷ) (?L . ?ᴸ)
    (?M . ?ᴹ) (?N . ?ᴺ) (?O . ?ᴼ) (?Ȣ . ?ᴽ)
    (?P . ?ᴾ) (?R . ?ᴿ) (?T . ?ᵀ) (?U . ?ᵁ)
    (?W . ?ᵂ) (?a . ?ᵃ) (?ɐ . ?ᵄ) (?ɑ . ?ᵅ)
    (?ᴂ . ?ᵆ) (?b . ?ᵇ) (?d . ?ᵈ) (?e . ?ᵉ)
    (?ə . ?ᵊ) (?ɛ . ?ᵋ) (?ɜ . ?ᵌ) (?g . ?ᵍ)
    (?k . ?ᵏ) (?m . ?ᵐ) (?ŋ . ?ᵑ) (?o . ?ᵒ)
    (?ɔ . ?ᵓ) (?ᴖ . ?ᵔ) (?ᴗ . ?ᵕ) (?p . ?ᵖ)
    (?t . ?ᵗ) (?u . ?ᵘ) (?ᴝ . ?ᵙ) (?ɯ . ?ᵚ)
    (?v . ?ᵛ) (?ᴥ . ?ᵜ) (?β . ?ᵝ) (?γ . ?ᵞ)
    (?δ . ?ᵟ) (?φ . ?ᵠ) (?χ . ?ᵡ) (?н . ?ᵸ)
    (?ɒ . ?ᶛ) (?c . ?ᶜ) (?ɕ . ?ᶝ) (?ð . ?ᶞ)
    (?ɜ . ?ᶟ) (?f . ?ᶠ) (?ɟ . ?ᶡ) (?ɡ . ?ᶢ)
    (?ɥ . ?ᶣ) (?ɨ . ?ᶤ) (?ɩ . ?ᶥ) (?ɪ . ?ᶦ)
    (?ᵻ . ?ᶧ) (?ʝ . ?ᶨ) (?ɭ . ?ᶩ) (?ᶅ . ?ᶪ)
    (?ʟ . ?ᶫ) (?ɱ . ?ᶬ) (?ɰ . ?ᶭ) (?ɲ . ?ᶮ)
    (?ɳ . ?ᶯ) (?ɴ . ?ᶰ) (?ɵ . ?ᶱ) (?ɸ . ?ᶲ)
    (?ʂ . ?ᶳ) (?ʃ . ?ᶴ) (?ƫ . ?ᶵ) (?ʉ . ?ᶶ)
    (?ʊ . ?ᶷ) (?ᴜ . ?ᶸ) (?ʋ . ?ᶹ) (?ʌ . ?ᶺ)
    (?z . ?ᶻ) (?ʐ . ?ᶼ) (?ʑ . ?ᶽ) (?ʒ . ?ᶾ)
    (?θ . ?ᶿ) (?0 . ?⁰) (?i . ?ⁱ) (?4 . ?⁴)
    (?5 . ?⁵) (?6 . ?⁶) (?7 . ?⁷) (?8 . ?⁸)
    (?9 . ?⁹) (?+ . ?⁺) (?− . ?⁻) (?= . ?⁼)
    (?( . ?⁽) (?) . ?⁾) (?n . ?ⁿ) (?ⵡ . ?ⵯ)))
 ;; ("SM" . ?℠) ("TM" . ?™)

(defsubst lookup-superscript-character (char)
  "Return the superscript character of CHAR if exists."
  (cdr (assq char lookup-superscript-char-table)))

(defun lookup-superscript-string (str)
  (let ((i (string-to-list str)) chars ch)
    (while i
      (if (setq ch (lookup-superscript-character (car i)))
          (setq chars (cons ch chars) i (cdr i))
        (setq i nil chars nil)))
    (if chars (apply 'string (nreverse chars))
      (put-text-property
       0 (length str)
       'display '((raise 0.3) (height 0.8))
       str)
      str)))

(defconst lookup-subscript-char-table
  '((?i . ?ᵢ) (?r . ?ᵣ) (?u . ?ᵤ) (?v . ?ᵥ)
    (?β . ?ᵦ) (?γ . ?ᵧ) (?ρ . ?ᵨ) (?φ . ?ᵩ)
    (?χ . ?ᵪ) (?0 . ?₀) (?1 . ?₁) (?2 . ?₂)
    (?3 . ?₃) (?4 . ?₄) (?5 . ?₅) (?6 . ?₆)
    (?7 . ?₇) (?8 . ?₈) (?9 . ?₉) (?+ . ?₊)
    (?− . ?₋) (?= . ?₌) (?( . ?₍) (?) . ?₎)
    (?a . ?ₐ) (?e . ?ₑ) (?o . ?ₒ) (?x . ?ₓ)
    (?ə . ?ₔ)))

(defsubst lookup-subscript-character (char)
  "Return the subscript character of CHAR if exists."
  (cdr (assq char lookup-subscript-char-table)))

(defun lookup-subscript-string (str)
  (let ((i (string-to-list str)) chars ch)
    (while i
      (if (setq ch (lookup-subscript-character (car i)))
          (setq chars (cons ch chars) i (cdr i))
        (setq i nil chars nil)))
    (if chars (apply 'string (nreverse chars))
      (put-text-property
       0 (length str)
       'display '((raise -0.3) (height 0.8))
       str)
      str)))

(provide 'lookup-text)

;;; lookup-text.el ends here
