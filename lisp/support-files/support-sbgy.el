;;; support-sbgy.el --- suport file for "宋本廣韻"
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

;; This support-file provides "宋本廣韻" XML dictionary seaching
;; capability.
;;
;; Text file can be downloaded from:
;; http://kanji-database.sourceforge.net/dict/sbgy/index.html

;;; Usage
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/sbgy.xml"
;;           ....))

;;; Code:

(defvar support-sbgy-voice-brackets
  '(("〖" . "〗") ;; 平声
    ("「" . "】") ;; 上声
    ("【" . "】") ;; 上声
    ("【" . "」"))) ;; 入声

(defvar support-sbgy-use-ivs-font t)

;;;
;;; Consonants and Vowels
;;;

(defvar support-sbgy-consonants
  '("p" "pʰ" "bʰ" "m" "t" "tʰ" "dʰ" "n" "ţ" "ţʰ" "ɖʰ" "ɳ" "ts" "tsʰ" "dzʰ" "s" "z" "ʧ" "ʧʰ" "dʒʰ" "ʃ" "dʐʰ" "tɕ" "tɕʰ" "dʑʰ" "ɕ" "ʑ" "k" "kʰ" "gʰ" "ŋ" "ʔ" "x" "ɣ" "" "j" "l" "nʑ" ))

(defvar support-sbgy-vowels
  '(("uŋ" nil nil nil "ĭuŋ" nil nil nil "1東" "1董" "1送" nil "1東" "1董" "1送" "1屋")
    ("uk" nil nil nil "ĭuk" nil nil nil nil nil nil "1屋")
    ("uoŋ" nil nil nil nil nil nil nil "2冬" nil "2宋" nil "2冬" "2腫" "2宋" "2沃")
    ("uok" nil nil nil nil nil nil nil nil nil nil "2沃")
    (nil nil nil nil "ĭwoŋ" nil nil nil "3鍾" "2腫" "3用" nil)
    (nil nil nil nil "ĭwok" nil nil nil nil nil nil "3燭")
    (nil nil "ɔŋ" nil nil nil nil nil "4江" "3講" "4絳" nil "3江" "3講" "3絳" "3覺")
    (nil nil "ɔk" nil nil nil nil nil nil nil nil "4覺")
    (nil nil nil nil "ĭe" "ĭwe" nil nil "5支" "4紙" "5寘" nil "4支" "4紙" "4寘" nil)
    (nil nil nil nil "i" "wi" nil nil "6脂" "5旨" "6至" nil)
    (nil nil nil nil "ĭə" nil nil nil "7之" "6止" "7志" nil)
    (nil nil nil nil "ĭəi" "ĭwəi" nil nil "8微" "7尾" "8未" nil "5微" "5尾" "5未")
    (nil nil nil nil "ĭo" nil nil nil "9魚" "8語" "9御" nil "6魚" "6語" "6御")
    (nil nil nil nil nil "ĭu" nil nil "10虞" "9麌" "10遇" nil "7虞" "7麌" "7遇")
    ("u" nil nil nil nil nil nil nil "11模" "10姥" "11暮" nil)
    (nil nil nil nil nil nil "iei" "iwei" "12齊" "11薺" "12霽" nil "8齊" "8薺" "8霽")
    (nil nil nil nil "ĭɛi" "ĭwɛi" nil nil nil nil "13祭" nil nil nil)
    ("ɑi" "uɑi" nil nil nil nil nil nil nil nil "14泰" nil "9泰")
    (nil nil "ai" "wai" nil nil nil nil "13佳" "12蟹" "15卦" nil "9佳" "9蟹" "10卦")
    (nil nil "ɐi" "wɐi" nil nil nil nil "14皆" "13駭" "16怪" nil)
    (nil nil "æi" "wæi" nil nil nil nil nil nil "17夬" nil nil nil)
    (nil "uɒi" nil nil nil nil nil nil "15灰" "14賄" "18隊" nil "10灰" "10賄" "11隊")
    ("ɒi" nil nil nil nil nil nil nil "16咍" "15海" "19代" nil)
    (nil nil nil nil "ĭɐi" "ĭwɐi" nil nil nil nil "20廢" nil nil nil)
    (nil nil nil nil "ĭĕn" "ĭwĕn" nil nil "17眞" "16軫" "21震" nil "11眞" "11軫" "12震" "4質")
    (nil nil nil nil "ĭĕt" "ĭwĕt" nil nil nil nil nil "5質")
    (nil nil nil nil nil "ĭuĕn" nil nil "18諄" "17準" "22稕" nil)
    (nil nil nil nil nil "ĭuĕt" nil nil nil nil nil "6術")
    (nil nil nil nil "ĭen" nil nil nil "19臻" nil nil nil nil nil)
    (nil nil nil nil "ĭet" nil nil nil nil nil nil "7櫛")
    (nil nil nil nil nil "ĭuən" nil nil "20文" "18吻" "23問" nil "12文" "12吻" "13問" "5物")
    (nil nil nil nil nil "ĭuət" nil nil nil nil nil "8物")
    (nil nil nil nil "ĭən" nil nil nil "21欣" "19隱" "24焮" nil)
    (nil nil nil nil "ĭət" nil nil nil nil nil nil "9迄")
    (nil "uən" nil nil nil nil nil nil "23魂" "21混" "26慁" nil "13元" "13阮" "14願" "6月")
    (nil "uət" nil nil nil nil nil nil nil nil nil "11沒")
    ("ən" nil nil nil nil nil nil nil "24痕" "22很" "27恨" nil)
    ("ət" nil nil nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil "ĭɐn" "ĭwɐn" nil nil "22元" "20阮" "25願" nil)
    (nil nil nil nil "ĭɐt" "ĭwɐt" nil nil nil nil nil "10月")
    ("ɑn" nil nil nil nil nil nil nil "25寒" "23旱" "28翰" nil "14寒" "14旱" "15翰" "7曷")
    ("ɑt" nil nil nil nil nil nil nil nil nil nil "12曷")
    (nil "uɑn" nil nil nil nil nil nil "26桓" "24緩" "29換" nil)
    (nil "uɑt" nil nil nil nil nil nil nil nil nil "13末")
    (nil nil "an" "wan" nil nil nil nil "27刪" "25潸" "30諌" nil "15刪" "15潸" "16諌" "8黠")
    (nil nil "at" "wat" nil nil nil nil nil nil nil "14黠")
    (nil nil "æn" "wæn" nil nil nil nil "28山" "26產" "31襉" nil)
    (nil nil "æt" "wæt" nil nil nil nil nil nil nil "15鎋")
    (nil nil nil nil nil nil "ien" "iwen" "1先" "27銑" "32霰" nil "1先" "16銑" "17霰" "9屑")
    (nil nil nil nil nil nil "iet" "iwet" nil nil nil "16屑")
    (nil nil nil nil "ĭɛn" "ĭwɛn" nil nil "2仙" "28獮" "33線" nil)
    (nil nil nil nil "ĭɛt" "ĭwɛt" nil nil nil nil nil "17薛")
    (nil nil nil nil nil nil "ieu" nil "3蕭" "29篠" "34嘯" nil "2蕭" "17篠" "18嘯" nil)
    (nil nil nil nil "ĭɛu" nil nil nil "4宵" "30小" "35笑" nil)
    (nil nil "au" nil nil nil nil nil "5肴" "31巧" "36效" nil "3肴" "18巧" "19效")
    ("ɑu" nil nil nil nil nil nil nil "6豪" "32皓" "37號" nil "4豪" "19皓" "20號")
    ("ɑ" nil nil nil nil nil nil nil "7歌" "33哿" "38箇" nil "5歌" "20哿" "21箇")
    (nil "uɑ" nil nil "ĭɑ" "ĭuɑ" nil nil "8戈" "34果" "39過" nil)
    (nil nil "a" "wa" "ĭa" nil nil nil "9麻" "35馬" "40禡" nil "6麻" "21馬" "22禡")
    (nil nil nil nil "ĭaŋ" "ĭwaŋ" nil nil "10陽" "36養" "41漾" nil "7陽" "22養" "23漾" "10薬")
    (nil nil nil nil "ĭak" "ĭwak" nil nil nil nil nil "18薬")
    ("ɑŋ" "uɑŋ" nil nil nil nil nil nil "11唐" "37蕩" "42宕" nil)
    ("ɑk" "uɑk" nil nil nil nil nil nil nil nil nil "19鐸")
    (nil nil "ɐŋ" "wɐŋ" "ĭɐŋ" "ĭwɐŋ" nil nil "12庚" "38梗" "43敬" nil "8庚" "23梗" "24敬" "11陌")
    (nil nil "ɐk" "wɐk" "ĭɐk" "ĭwɐk" nil nil nil nil nil "20陌")
    (nil nil "æŋ" "wæŋ" nil nil nil nil "13耕" "39耿" "44諍" nil)
    (nil nil "æk" "wæk" nil nil nil nil nil nil nil "21麥")
    (nil nil nil nil "ĭɛŋ" "ĭwɛŋ" nil nil "14清" "40靜" "45勁" nil)
    (nil nil nil nil "ĭɛk" "ĭwɛk" nil nil nil nil nil "22昔")
    (nil nil nil nil nil nil "ieŋ" "iweŋ" "15靑" "41迥" "46徑" nil "9靑" "24迥" "25徑" "12錫")
    (nil nil nil nil nil nil "iek" "iwek" nil nil nil "23錫")
    (nil nil nil nil "ĭəŋ" nil nil nil "16蒸" "42拯" "47證" nil "10蒸" nil nil "13職")
    (nil nil nil nil "ĭək" "ĭwək" nil nil nil nil nil "24職")
    ("əŋ" "uəŋ" nil nil nil nil nil nil "17登" "43等" "48嶝" nil)
    ("ək" "uək" nil nil nil nil nil nil nil nil nil "25德")
    (nil nil nil nil "ĭəu" nil nil nil "18尤" "44有" "49宥" nil "11尤" "25有" "26宥" nil)
    ("əu" nil nil nil nil nil nil nil "19侯" "45厚" "50候" nil)
    (nil nil nil nil "iəu" nil nil nil "20幽" "46黝" "51幼" nil)
    (nil nil nil nil "ĭĕm" nil nil nil "21侵" "47寑" "52沁" nil "12侵" "26寑" "27沁" "14緝")
    (nil nil nil nil "ĭĕp" nil nil nil nil nil nil "26緝")
    ("ɒm" nil nil nil nil nil nil nil "22覃" "48感" "53勘" nil "13覃" "27感" "28勘" "15合")
    ("ɒp" nil nil nil nil nil nil nil nil nil nil "27合")
    ("ɑm" nil nil nil nil nil nil nil "23談" "49敢" "54鬫" nil)
    ("ɑp" nil nil nil nil nil nil nil nil nil nil "28盍")
    (nil nil nil nil "ĭɛm" nil nil nil "24鹽" "50琰" "55豔" nil "14鹽" "28琰" "29豔" "16葉")
    (nil nil nil nil "ĭɛp" nil nil nil nil nil nil "29葉")
    (nil nil nil nil nil nil "iem" nil "25添" "51忝" "56㮇" nil)
    (nil nil nil nil nil nil "iep" nil nil nil nil "30帖")
    (nil nil "ɐm" nil nil nil nil nil "26咸" "53豏" "58陥" nil "15咸" "29豏" "30陥" "17洽")
    (nil nil "ɐp" nil nil nil nil nil nil nil nil "31洽")
    (nil nil "am" nil nil nil nil nil "27銜" "54檻" "59鑑" nil)
    (nil nil "ap" nil nil nil nil nil nil nil nil "32狎")
    (nil nil nil nil "ĭɐm" nil nil nil "28嚴" "52儼" "57釅" nil)
    (nil nil nil nil "ĭɐp" nil nil nil nil nil nil "33業")
    (nil nil nil nil nil "ĭwɐm" nil nil "29凡" "55梵" "60范" nil)
    (nil nil nil nil nil "ĭwɐp" nil nil nil nil nil "34乏")))

(defun support-sbgy-first-8-elems (list)
  (let ((x (copy-seq list)))
    (setcdr (nthcdr 7 x) nil)
    x))

;; e.g. '(1 2 3 4) & (nil 3 nil 7) -> '(1 3 3 7)
(defun support-sbgy-renew-list (old-list new-list)
  (let (result)
    (dotimes (i (length old-list))
      (setq result (cons (or (car new-list) (car old-list)) 
                         result)
            old-list (cdr old-list) 
            new-list (cdr new-list)))
    (nreverse result)))

(defvar support-sbgy-pronunciation-regexp
  (concat 
   "^\\(" (regexp-opt support-sbgy-consonants)
   "\\)\\("
   (regexp-opt
    (apply 'nconc
           (mapcar 'support-sbgy-first-8-elems
                   support-sbgy-vowels)))
   "\\)\\("
   "[˥˩]*\\)$"))

(defvar support-sbgy-vowel-to-letter
  (let ((new-list '(nil nil nil nil nil nil nil nil)))
    (remove-if
     'null
     (apply 
      'nconc
      (mapcar 
       (lambda (x) 
         (mapcar 
          (lambda (y) (if y (cons y (setq new-list (support-sbgy-renew-list 
                                                    new-list (nthcdr 8 x))))))
          (support-sbgy-first-8-elems x)))
       support-sbgy-vowels)))))

(defvar support-sbgy-vowel-groups
  (remove-if (lambda (x) (< (length x) 2))
  (apply 'nconc
  (mapcar 
   (lambda (voice)
     (let (vowel-groups vowel-group
           current-guangyun
           current-pingshui)
       (dolist (vowel support-sbgy-vowels)
         (setq current-guangyun (elt vowel (+ voice 7)))
         (setq current-pingshui (elt vowel (+ voice 11)))
         (when current-pingshui
           (setq vowel-groups (cons (nreverse (remove-if 'null vowel-group)) vowel-groups)
                 vowel-group nil))
         (when current-guangyun
           (setq vowel-group (append (support-sbgy-first-8-elems vowel)
                                     vowel-group))))
       (nreverse (cons (nreverse (remove-if 'null vowel-group)) vowel-groups))))
   '(3 4)))))

(defun support-sbgy-vowel-to-letter (vowel voice)
  (let* ((letters (assoc vowel support-sbgy-vowel-to-letter))
         (guangyun (elt letters (1+ voice)))
         (pingshui (elt letters (+ 5 voice)))
         (brckts   (elt support-sbgy-voice-brackets voice)))
    (format "%s%s/%s%s"
            (car brckts) guangyun pingshui (cdr brckts))))

;; e.g. kĭɛi →  kɑi,kuɑi,kĭɛi,kĭwɛi,kiei,kiwei 
;(defun support-sbgy-query-filter (query)
;  (let* ((string (lookup-query-string query)))
;    (if (string-match support-sbgy-pronunciation-regexp string)
;        (let ((consonant (match-string 1 string))
;              (vowel     (match-string 2 string))
;              (voice     (match-string 3 string))
;              (vowel-groups support-sbgy-vowel-groups)
;              queries)
;          (while vowel-groups
;            (if (member vowel (car vowel-groups))
;                (setq queries
;                      (mapcar (lambda (v)
;                                (lookup-new-query 
;                                 (lookup-query-method query)
;                                 (concat consonant v voice)
;                                 (concat consonant v voice)))
;                              (car vowel-groups))
;                      vowel-groups nil)
;              (setq vowel-groups (cdr vowel-groups))))
;          (if queries queries query))
;      query)))

(defun support-sbgy-entry-tags-list (string method)
  (if (and (equal method 'text)
           (lookup-text-cjk-p string))
      '(("" . ""))
    (cond ((lookup-text-charsetsp string '(han))
           '((">" . "<note>")
             (">" . "<added_note>")
             (">" . "<headnote>")
             (">" . "<rewrite_word>")
             (">" . "</rewrite_word>")))
          ((string-match "^[ -˿]+$" string)
           '(("ipa=\"" . "\"")))
          ((string-match "^[ア-ン]+$" string)
           '(("onyomi=\"" . "\"")))
          (t nil))))

;(defun support-sbgy-head-tags (x)
;  (let* ((ipa (ndsary-extract-string x "ipa=\"" "\""))
;         (yomi (ndsary-extract-string x "onyomi=\"" "\""))
;         (char (ndsary-extract-string x ">" "<note"))
;         (vowel (progn (string-match support-sbgy-pronunciation-regexp ipa)
;                       (match-string 2 ipa)))
;         (voice (cond ((string-match "˩$" ipa) 0)
;                      ((string-match "˩˥$" ipa) 2)
;                      ((string-match "˥$" ipa) 1)
;                      (t 3)))
;         (letter (support-sbgy-vowel-to-letter vowel voice)))
;    (format "%s%s%s%s" letter char yomi ipa)))

(defun support-sbgy-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (if support-sbgy-use-ivs-font
      (lookup-text-new-to-old-kanji-ivs-region (point-min) (point-max)))
  (goto-char (point-min))
  (while (re-search-forward ">\\([^>]+\\)<\\(note\\|added_note\\|headnote\\|\\rewrite_word\\|/rewrite_word\\)>" nil t)
    (add-text-properties (match-beginning 1) (match-end 1) 
                         '(display ((height 2.0)) face lookup-heading-1-face)))
  (while (re-search-forward "<original_word>\\([^>]+\\)<" nil t)
    (add-text-properties (match-beginning 1) (match-end 1) 
                         '(display ((height 2.0)) face lookup-comment-face)))
  (goto-char (point-min))
  (while (re-search-forward "<original_text>\\([^>]+\\)<" nil t)
    (add-text-properties (match-beginning 1) (match-end 1) 
                         '(face lookup-comment-face)))
  (goto-char (point-min))
  (while (re-search-forward "\n+\\|\t+\\|<[^>]+>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (when (re-search-forward "[㐀-鿿𠀀-𯟿]" nil t)
    (goto-char (point-min))
    (insert (match-string 0) "\n")))

(setq lookup-support-options
      (list :title "宋本廣韻"
            ;; :query-filter 'support-sbgy-query-filter
            :entry-tags-list 'support-sbgy-entry-tags-list
            :content-tags '("<voice_part" . "</voice_part>")
            :head-tags '("\">" .  "<note>")
            :code-tags '("id=\"" . "\">")
            :arranges '((structure support-sbgy-arrange-structure))))

;;; support-sbgy.el ends here
