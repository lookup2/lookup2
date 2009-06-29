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

(defun lookup-query-filter-normalize-nfc (query)
  (if (lookup-query-string query)
      (setf (lookup-query-string query)
            (replace-regexp-in-string 
             "[〾󠀀-󯿽]" "" (ucs-normalize-NFC-string 
                           (lookup-query-string query)))))
  query)

(defun lookup-query-filter-normalize-nfkc (query)
  (if (lookup-query-string query)
      (setf (lookup-query-string query)
            (replace-regexp-in-string 
             "[〾󠀀-󯿽]" "" (ucs-normalize-NFKC-string 
                           (lookup-query-string query)))))
  query)

(add-to-list 'lookup-query-filters 'lookup-query-filter-normalize-nfkc)

;;
;; case
;;

(defun lookup-query-filter-downcase (query)
  (setf (lookup-query-string query)
        (downcase (lookup-query-string query)))
  query)

(add-to-list 'lookup-query-filters 'lookup-query-filter-downcase)

;;
;; Stem English
;;

(defun lookup-query-filter-stem-english (query)
  (let* ((string (downcase (lookup-query-string query)))
         (method (lookup-query-method query))
         strings)
    (if (or (equal method 'exact) (equal method 'keyword))
        (mapcar 
         (lambda (x) (lookup-new-query method x))
         (cons string
               (remove-if 
                (lambda (x) (< (length x) 4))
                (cdr (nreverse (stem-english string))))))
      query)))

(defun lookup-query-filter-kanji-to-kana (query)
  (mapcar (lambda (x) (lookup-new-query (lookup-query-method query) x))
          (lookup-text-get-readings (lookup-query-string query))))

(defun lookup-query-filter-hiragana-to-katakana (query)
  (setf (lookup-query-string query)
        (japanese-katakana (lookup-query-string query)))
  query)

(defun lookup-decompose-alphabet-chars (chars)
  "Decompose alphabet characters CHARS."
  (apply 'append
         (mapcar '(lambda (x)
                    (or (and (< x #x2000)
                             (get-char-code-property x 'decomposition))
                        (list x)))
                 chars)))

(defun lookup-remove-alphabet-accents (string)
  (let* ((chars (string-to-list string))
         (new-chars (lookup-decompose-alphabet-chars chars)))
    (while (/= (length chars) (length new-chars))
      (setq chars new-chars)
      (setq new-chars
            (remove-if 
             (lambda (x)
               (let ((ccc 
                      (get-char-code-property x 'canonical-combining-class)))
                 (and ccc (< 0 ccc))))
             (lookup-decompose-alphabet-chars new-chars))))
    (apply 'string chars)))

(defun lookup-query-filter-remove-accents (query)
  (setf (lookup-query-string query)
        (lookup-remove-alphabet-accents (lookup-query-string query)))
  query)

;; remove from list if you don't like.
;; (add-to-list 'lookup-query-filters 'lookup-query-filter-remove-accents)


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

(defvar lookup-text-new-old-ivs-alist
  '((?唖 . "啞") (?逢 . "逢󠄁") (?穐 . "龝") (?芦 . "芦󠄁")
    (?扱 . "扱󠄁") (?飴 . "飴󠄁") (?暗 . "暗󠄁") (?偉 . "偉󠄁")
    (?囲 . "圍󠄁") (?意 . "意󠄁") (?緯 . "緯󠄁") (?違 . "違󠄂")
    (?遺 . "遺󠄁") (?磯 . "磯󠄁") (?溢 . "溢󠄁") (?逸 . "逸󠄁")
    (?茨 . "茨󠄁") (?鰯 . "鰯󠄁") (?淫 . "淫󠄁") (?羽 . "羽󠄀")
    (?迂 . "迂󠄁") (?嘘 . "噓") (?厩 . "厩󠄂") (?閏 . "𨳝󠄀")
    (?噂 . "噂󠄁") (?運 . "運󠄁") (?餌 . "餌󠄁") (?鋭 . "銳")
    (?益 . "益󠄁") (?悦 . "悅") (?謁 . "謁󠄀") (?閲 . "閱")
    (?延 . "延󠄂") (?援 . "援󠄁") (?沿 . "沿󠄀") (?焔 . "焰")
    (?縁 . "緣") (?艶 . "艷") (?遠 . "遠󠄁") (?鉛 . "鉛󠄂")
    (?於 . "於󠄁") (?往 . "往󠄁") (?横 . "橫") (?翁 . "翁󠄂")
    (?襖 . "襖󠄁") (?鴎 . "鷗") (?黄 . "黃") (?温 . "溫")
    (?音 . "音󠄁") (?化 . "花󠄁") (?禍 . "禍󠄀") (?花 . "花󠄁")
    (?貨 . "貨󠄁") (?迦 . "迦󠄁") (?過 . "過󠄁") (?蚊 . "蚊󠄁")
    (?芽 . "芽󠄀") (?雅 . "雅󠄂") (?餓 . "餓󠄁") (?廻 . "𢌞󠄀")
    (?悔 . "悔󠄀") (?恢 . "恢󠄁") (?拐 . "拐󠄁") (?晦 . "晦󠄁")
    (?海 . "海󠄀") (?灰 . "灰󠄁") (?害 . "害󠄂") (?慨 . "慨󠄁")
    (?概 . "槪") (?較 . "較󠄁") (?隔 . "隔󠄁") (?割 . "割󠄀")
    (?喝 . "喝󠄁") (?渇 . "渴") (?葛 . "葛󠄁") (?褐 . "賓󠄀")
    (?轄 . "轄󠄁") (?鞄 . "鞄󠄁") (?鎌 . "鎌󠄁") (?噛 . "嚙")
    (?寒 . "寒󠄁") (?漢 . "漢󠄁") (?環 . "環󠄁") (?緩 . "緩󠄁")
    (?翰 . "翰󠄁") (?還 . "還󠄁") (?韓 . "韓󠄁") (?館 . "館󠄁")
    (?舘 . "舘󠄁") (?翫 . "翫󠄁") (?危 . "危󠄁") (?器 . "器󠄁")
    (?既 . "既󠄀") (?期 . "期󠄁") (?祈 . "祈󠄀") (?徽 . "徽󠄁")
    (?起 . "起󠄁") (?飢 . "飢󠄁") (?亀 . "龜") (?喫 . "喫󠄁")
    (?脚 . "腳") (?虐 . "虐󠄁") (?逆 . "逆󠄁") (?及 . "及󠄁")
    (?吸 . "吸󠄁") (?急 . "急󠄁") (?汲 . "汲󠄁") (?笈 . "笈󠄁")
    (?級 . "級󠄁") (?巨 . "巨󠄁") (?拒 . "拒󠄁") (?虚 . "虛")
    (?距 . "距󠄁") (?卿 . "卿󠄂") (?強 . "强") (?恐 . "恐󠄁")
    (?教 . "敎") (?郷 . "鄕") (?響 . "響󠄃") (?饗 . "饗󠄂")
    (?僅 . "僅󠄁") (?勤 . "勤󠄁") (?謹 . "謹󠄀") (?近 . "近󠄁")
    (?倶 . "俱") (?矩 . "矩󠄁") (?躯 . "軀") (?具 . "具󠄁")
    (?虞 . "虞󠄁") (?喰 . "喰󠄁") (?空 . "空󠄁") (?遇 . "遇󠄁")
    (?櫛 . "㵎󠄀") (?屑 . "屑󠄁") (?靴 . "靴󠄁") (?薫 . "薰")
    (?祁 . "祁󠄁") (?啓 . "啓󠄁") (?契 . "契󠄁") (?慧 . "慧󠄂")
    (?掲 . "揭") (?携 . "擕") (?繋 . "繫") (?迎 . "迎󠄁")
    (?撃 . "擊") (?隙 . "𨻶󠄀") (?潔 . "潔󠄁") (?穴 . "穴󠄁")
    (?月 . "月󠄁") (?倦 . "倦󠄁") (?健 . "健󠄁") (?兼 . "兼󠄁")
    (?券 . "券󠄁") (?嫌 . "嫌󠄁") (?建 . "建󠄁") (?憲 . "憲󠄁")
    (?拳 . "拳󠄁") (?捲 . "捲󠄁") (?研 . "硏") (?肩 . "肩󠄁")
    (?謙 . "謙󠄁") (?遣 . "遣󠄁") (?鹸 . "鹼") (?諺 . "諺󠄁")
    (?戸 . "戶") (?雇 . "雇󠄁") (?顧 . "顧󠄁") (?呉 . "吳")
    (?娯 . "娛") (?誤 . "誤󠄁") (?交 . "交󠄁") (?公 . "公󠄁")
    (?巷 . "巷󠄁") (?慌 . "慌󠄁") (?控 . "控󠄁") (?更 . "更󠄁")
    (?校 . "校󠄁") (?構 . "構󠄁") (?浩 . "浩󠄁") (?港 . "港󠄁")
    (?溝 . "溝󠄁") (?硬 . "硬󠄁") (?絞 . "絞󠄁") (?耕 . "耕󠄁")
    (?考 . "考󠄁") (?腔 . "腔󠄁") (?荒 . "荒󠄁") (?講 . "講󠄁")
    (?購 . "購󠄂") (?麹 . "麴") (?告 . "吿") (?穀 . "穀󠄀")
    (?酷 . "酷󠄁") (?鵠 . "鵠󠄁") (?黒 . "黑") (?腰 . "腰󠄁")
    (?甑 . "甑󠄂") (?込 . "込󠄁") (?鎖 . "鎖󠄁") (?彩 . "彩󠄁")
    (?採 . "採󠄁") (?歳 . "歲") (?采 . "采󠄁") (?菜 . "菜󠄁")
    (?榊 . "榊󠄁") (?咲 . "咲󠄁") (?削 . "削󠄁") (?柵 . "栅")
    (?殺 . "殺󠄀") (?鯖 . "鯖󠄁") (?錆 . "錆󠄁") (?珊 . "珊󠄁")
    (?産 . "產") (?使 . "使󠄁") (?史 . "史󠄁") (?姉 . "姊")
    (?姿 . "姿󠄁") (?祉 . "祉󠄁") (?視 . "視󠄁") (?諮 . "諮󠄁")
    (?資 . "資󠄁") (?飼 . "飼󠄁") (?次 . "次󠄁") (?屡 . "屢")
    (?捨 . "捨󠄁") (?斜 . "斜󠄁") (?煮 . "煮󠄀") (?社 . "社󠄁")
    (?者 . "者󠄁") (?遮 . "遮󠄁") (?邪 . "邪󠄂") (?勺 . "勺󠄀")
    (?杓 . "杓󠄁") (?灼 . "灼󠄁") (?爵 . "爵󠄂") (?酌 . "酌󠄁")
    (?弱 . "弱󠄁") (?主 . "主󠄁") (?周 . "周󠄀") (?終 . "終󠄁")
    (?繍 . "繡") (?習 . "習󠄁") (?臭 . "祝󠄀") (?週 . "週󠄁")
    (?酋 . "酋󠄁") (?住 . "住󠄁") (?祝 . "祝󠄀") (?術 . "術󠄁")
    (?述 . "述󠄁") (?潤 . "弸󠄁") (?巡 . "巡󠄁") (?遵 . "遵󠄁")
    (?所 . "所󠄁") (?暑 . "暑󠄁") (?曙 . "曙󠄁") (?渚 . "渚󠄀")
    (?緒 . "緖") (?署 . "署󠄀") (?薯 . "薯󠄁") (?藷 . "藷󠄁")
    (?諸 . "諸󠄀") (?勝 . "勝󠄁") (?哨 . "哨󠄁") (?宵 . "宵󠄁")
    (?尚 . "尙") (?松 . "松󠄁") (?梢 . "梢󠄁") (?消 . "所󠄁")
    (?渉 . "涉") (?硝 . "硝󠄁") (?祥 . "祥󠄀") (?肖 . "肖󠄁")
    (?蒋 . "蔣") (?訟 . "訟󠄁") (?醤 . "醬") (?鞘 . "鞘󠄁")
    (?丈 . "𠀋󠄀") (?城 . "城󠄀") (?情 . "情󠄁") (?状 . "狀")
    (?飾 . "飾󠄁") (?食 . "食󠄁") (?蝕 . "蝕󠄁") (?侵 . "侵󠄁")
    (?浸 . "浸󠄁") (?神 . "神󠄀") (?進 . "進󠄁") (?尋 . "尋󠄁")
    (?訊 . "訊󠄂") (?迅 . "迅󠄁") (?逗 . "逗󠄁") (?翠 . "翠󠄁")
    (?遂 . "遂󠄂") (?摺 . "摺󠄁") (?瀬 . "瀨") (?成 . "成󠄁")
    (?晴 . "晴󠄀") (?清 . "淸") (?盛 . "盛󠄁") (?精 . "精󠄀")
    (?聖 . "聖󠄁") (?誠 . "誠󠄁") (?請 . "請󠄁") (?逝 . "逝󠄁")
    (?青 . "靑") (?税 . "稅") (?脆 . "脆󠄁") (?隻 . "隻󠄁")
    (?籍 . "籍󠄁") (?節 . "節󠄁") (?説 . "說") (?雪 . "雪󠄁")
    (?絶 . "絕") (?蝉 . "蟬") (?扇 . "扇󠄁") (?撰 . "撰󠄁")
    (?栓 . "栓󠄁") (?煎 . "煎󠄁") (?煽 . "煽󠄁") (?箭 . "箭󠄁")
    (?羨 . "羡󠄀") (?舛 . "舛󠄁") (?船 . "船󠄂") (?詮 . "詮󠄁")
    (?選 . "選󠄁") (?遷 . "遷󠄁") (?前 . "前󠄁") (?全 . "全󠄁")
    (?噌 . "噌󠄁") (?祖 . "祖󠄁") (?遡 . "遡󠄁") (?僧 . "僧󠄁")
    (?層 . "層󠄁") (?掻 . "搔") (?巣 . "巢") (?痩 . "瘦")
    (?送 . "送󠄁") (?遭 . "遭󠄁") (?増 . "增") (?憎 . "憎󠄀")
    (?贈 . "贈󠄁") (?造 . "造󠄁") (?即 . "卽") (?速 . "速󠄁")
    (?賊 . "賊󠄁") (?揃 . "揃󠄁") (?尊 . "尊󠄁") (?遜 . "遜󠄁")
    (?妥 . "妥󠄁") (?騨 . "驒") (?腿 . "腿󠄁") (?退 . "退󠄁")
    (?逮 . "逮󠄁") (?隊 . "隊󠄁") (?黛 . "黛󠄁") (?啄 . "硺󠄀")
    (?濯 . "濯󠄁") (?琢 . "琢󠄀") (?蛸 . "蛸󠄁") (?達 . "達󠄁")
    (?脱 . "脫") (?巽 . "巽󠄀") (?辿 . "辿󠄁") (?棚 . "栅󠄀")
    (?鱈 . "鱈󠄁") (?樽 . "樽󠄁") (?嘆 . "嘆󠄀") (?歎 . "歎󠄁")
    (?炭 . "炭󠄁") (?箪 . "簞") (?誕 . "誕󠄂") (?暖 . "暖󠄁")
    (?逐 . "逐󠄁") (?着 . "著󠄁") (?柱 . "柱󠄁") (?注 . "注󠄁")
    (?註 . "註󠄁") (?駐 . "駐󠄁") (?瀦 . "瀦󠄁") (?猪 . "猪󠄀")
    (?著 . "著󠄁") (?凋 . "凋󠄁") (?彫 . "彫󠄁") (?徴 . "徵")
    (?懲 . "懲󠄁") (?朝 . "朝󠄁") (?潮 . "潮󠄀") (?調 . "調󠄁")
    (?捗 . "捗󠄁") (?朕 . "朕󠄂") (?墜 . "墜󠄁") (?槌 . "槌󠄁")
    (?追 . "追󠄁") (?鎚 . "鎚󠄁") (?通 . "通󠄁") (?塚 . "塚󠄁")
    (?掴 . "摑") (?辻 . "辻󠄁") (?坪 . "坪󠄁") (?釣 . "釣󠄁")
    (?鶴 . "鶴󠄀") (?呈 . "呈󠄁") (?帝 . "帝󠄁") (?庭 . "庭󠄁")
    (?廷 . "廷󠄁") (?禎 . "禎󠄁") (?程 . "程󠄁") (?艇 . "艇󠄁")
    (?鄭 . "鄭󠄁") (?擢 . "擢󠄁") (?的 . "的󠄁") (?溺 . "溺󠄁")
    (?迭 . "迭󠄁") (?填 . "塡") (?添 . "添󠄁") (?顛 . "顚")
    (?兎 . "兎󠄁") (?堵 . "堵󠄁") (?屠 . "屠󠄁") (?菟 . "菟󠄁")
    (?賭 . "賭󠄁") (?途 . "途󠄁") (?都 . "都󠄀") (?冬 . "冬󠄀")
    (?唐 . "唐󠄁") (?塘 . "塘󠄁") (?祷 . "禱") (?糖 . "糖󠄀")
    (?藤 . "藤󠄁") (?謄 . "謄󠄁") (?逃 . "逃󠄂") (?透 . "透󠄁")
    (?騰 . "騰󠄁") (?導 . "導󠄁") (?道 . "道󠄁") (?徳 . "德")
    (?涜 . "瀆") (?栃 . "擔") (?突 . "突󠄁") (?瀞 . "瀞󠄁")
    (?遁 . "遁󠄁") (?呑 . "吞") (?内 . "內") (?謎 . "謎󠄁")
    (?灘 . "灘󠄁") (?楢 . "楢󠄁") (?難 . "難󠄀") (?肉 . "肉󠄁")
    (?乳 . "乳󠄁") (?忍 . "忍󠄁") (?認 . "認󠄁") (?禰 . "禰󠄁")
    (?祢 . "禰󠄁") (?寧 . "寧󠄀") (?葱 . "蔥") (?嚢 . "囊")
    (?納 . "納󠄁") (?覇 . "覇󠄁") (?派 . "派󠄂") (?罵 . "駡")
    (?排 . "排󠄁") (?牌 . "牌󠄁") (?輩 . "輩󠄁") (?梅 . "梅󠄀")
    (?這 . "這󠄁") (?秤 . "秤󠄁") (?剥 . "剝") (?博 . "博󠄁")
    (?薄 . "薄󠄁") (?迫 . "迫󠄁") (?縛 . "縛󠄁") (?箸 . "箸󠄁")
    (?肇 . "肇󠄁") (?溌 . "潑") (?醗 . "醱") (?伴 . "伴󠄁")
    (?判 . "判󠄁") (?半 . "半󠄁") (?叛 . "叛󠄁") (?帆 . "帆󠄁")
    (?畔 . "畔󠄁") (?繁 . "繁󠄁") (?飯 . "飯󠄀") (?挽 . "挽󠄁")
    (?晩 . "晚") (?卑 . "卑󠄀") (?悲 . "悲󠄁") (?扉 . "扉󠄂")
    (?斐 . "斐󠄁") (?碑 . "碑󠄀") (?緋 . "緋󠄁") (?誹 . "誹󠄁")
    (?避 . "避󠄁") (?樋 . "樋󠄁") (?鼻 . "鼻󠄁") (?柊 . "柊󠄁")
    (?稗 . "稗󠄁") (?彦 . "彥") (?逼 . "逼󠄁") (?姫 . "姬󠄁")
    (?媛 . "媛󠄁") (?謬 . "謬󠄁") (?評 . "評󠄁") (?豹 . "豹󠄁")
    (?廟 . "廟󠄁") (?病 . "病󠄁") (?瀕 . "瀕󠄁") (?貧 . "貧󠄁")
    (?頻 . "頻󠄀") (?敏 . "敏󠄀") (?瓶 . "甁") (?婦 . "婦󠄁")
    (?敷 . "敷󠄁") (?普 . "普󠄁") (?浮 . "浮󠄁") (?父 . "父󠄁")
    (?譜 . "譜󠄁") (?侮 . "侮󠄁") (?服 . "服󠄁") (?福 . "福󠄁")
    (?覆 . "覆󠄁") (?分 . "分󠄁") (?粉 . "粉󠄁") (?紛 . "紛󠄁")
    (?雰 . "雰󠄁") (?文 . "文󠄁") (?丙 . "丙󠄁") (?塀 . "塀󠄁")
    (?幣 . "幣󠄁") (?弊 . "弊󠄁") (?柄 . "柄󠄁") (?蔽 . "蔽󠄂")
    (?瞥 . "瞥󠄁") (?蔑 . "蔑󠄁") (?偏 . "偏󠄁") (?篇 . "篇󠄁")
    (?編 . "編󠄁") (?返 . "返󠄁") (?遍 . "遍󠄁") (?便 . "便󠄁")
    (?勉 . "勉󠄀") (?娩 . "娩󠄁") (?歩 . "步") (?簿 . "簿󠄁")
    (?包 . "包󠄁") (?崩 . "崩󠄁") (?庖 . "庖󠄁") (?抱 . "抱󠄁")
    (?朋 . "朋󠄁") (?泡 . "泡󠄁") (?砲 . "砲󠄁") (?縫 . "縫󠄁")
    (?胞 . "胞󠄁") (?萌 . "萌󠄁") (?蓬 . "蓬󠄁") (?邦 . "邦󠄂")
    (?飽 . "飽󠄁") (?鵬 . "鵬󠄁") (?亡 . "亡󠄁") (?帽 . "帽󠄁")
    (?忘 . "忘󠄁") (?忙 . "忙󠄁") (?房 . "房󠄁") (?望 . "望󠄂")
    (?冒 . "冒󠄁") (?頬 . "頰") (?墨 . "墨󠄀") (?翻 . "翻󠄁")
    (?凡 . "凡󠄁") (?盆 . "盆󠄁") (?摩 . "摩󠄁") (?磨 . "磨󠄁")
    (?魔 . "魔󠄁") (?麻 . "麻󠄁") (?毎 . "每") (?鱒 . "鱒󠄁")
    (?俣 . "㑨󠄀") (?迄 . "迄󠄁") (?麿 . "麿󠄁") (?脈 . "脈󠄂")
    (?明 . "明󠄁") (?迷 . "迷󠄁") (?免 . "免󠄁") (?麺 . "麵")
    (?妄 . "妄󠄁") (?盲 . "盲󠄁") (?耗 . "耗󠄁") (?儲 . "儲󠄁")
    (?餅 . "餅󠄁") (?戻 . "戾") (?籾 . "籾󠄁") (?紋 . "絞󠄁")
    (?約 . "約󠄁") (?躍 . "躍󠄁") (?靖 . "靖󠄁") (?鑓 . "鑓󠄁")
    (?愉 . "愉󠄁") (?愈 . "愈󠄁") (?癒 . "癒󠄁") (?諭 . "諭󠄀")
    (?輸 . "輸󠄁") (?有 . "有󠄁") (?猶 . "猶󠄁") (?猷 . "猷󠄂")
    (?祐 . "祐󠄀") (?曜 . "曜󠄁") (?耀 . "耀󠄁") (?要 . "要󠄁")
    (?養 . "養󠄁") (?翌 . "翌󠄁") (?翼 . "翼󠄂") (?莱 . "萊")
    (?頼 . "賴") (?欄 . "欄󠄀") (?蘭 . "蘭󠄁") (?吏 . "吏󠄁")
    (?率 . "率󠄁") (?隆 . "隆󠄁") (?旅 . "旅󠄁") (?虜 . "虜󠄀")
    (?遼 . "遼󠄁") (?緑 . "綠") (?燐 . "燐󠄁") (?隣 . "隣󠄂")
    (?鱗 . "鱗󠄁") (?麟 . "麟󠄂") (?涙 . "淚") (?類 . "類󠄀")
    (?暦 . "曆") (?歴 . "歷") (?廉 . "廉󠄀") (?憐 . "憐󠄁")
    (?漣 . "漣󠄀") (?煉 . "煉󠄀") (?簾 . "廉󠄀") (?練 . "練󠄀")
    (?蓮 . "蓮󠄀") (?連 . "連󠄀") (?錬 . "鍊") (?榔 . "榔󠄁")
    (?蝋 . "蠟") (?郎 . "郞") (?録 . "錄")))

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

(defun lookup-query-filter-old-to-new-kanji (query)
  (setf (lookup-query-string query)
        (lookup-text-old-to-new (lookup-query-string query)))
  query)

(add-to-list 'lookup-query-filters 'lookup-query-filter-old-to-new-kanji)

(defun lookup-text-new-to-old-kanji-ivs (str)
  "Convert new-style Kanji to old-style Kanji."
  (apply 
   'concat
   (mapcar (lambda (x) (or (cdr (assoc x lookup-text-new-old-ivs-alist)) 
                           (list x)))
           (string-to-list str))))

(defun lookup-text-new-to-old-kanji-ivs-region (from to)
  "Convert new-style Kanji to old-style Kanji in REGION."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "[㐀-鿿𠀀-𬿿]+" nil t)
        (replace-match (lookup-text-new-to-old-kanji-ivs (match-string 0)))))))

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
