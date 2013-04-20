;;; tibetan-util+.el --- Tibetan Utility  -*- lexical-binding: t -*-

;; Copyright (c) KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Tibetan Text utility
;; Created: 03/01/2013
;; Keywords: Tibetan
;; Version: 0.0.1
;; Package-version: 0.0.1
;; URL: http://github.com/lookup2/

;;; Commentary:

;; This provides transliteration and misc. utilities for Tibetan
;; language.

;;; Code:

;;; Basic Tibetan Data

(defvar tibetan-characters
  '(
    (;;consonants
     ?ཀ ?ཁ ?ག ?ང ;; `གྷ' ≡ `གྷ'
     ?ཅ ?ཆ ?ཇ ?ཉ
     ?ཊ ?ཋ ?ཌ ?ཎ ;; `ཌྷ' ≡ `ཌྷ'
     ?ཏ ?ཐ ?ད ?ན ;; `དྷ' ≡ `དྷ'
     ?པ ?ཕ ?བ ?མ ;; `བྷ' ≡ `བྷ'
     ?ཙ ?ཚ ?ཛ ?ཝ ;; `ཛྷ' ≡ `ཛྷ'
     ?ཞ ?ཟ ?འ ?ཡ ?ར ?ལ
     ?ཤ ?ཥ ?ས ?ཧ) ;; `ཀྵ' ≡ `ཀྵ' (?ཪ fixed R is not supported.)
    (;; inherent vowel
     ?ཨ)
    (;; vowels
     ?ཱ ?ི ?ཱི ?ུ ?ཱུ
     ?ྲྀ ?ཷ ?ླྀ ?ཹ 
     ?ེ ?ཻ ?ོ ?ཽ
     ?ྀ ?ཱྀ)
    (;; vmodifier
     ?ཾ ?ཿ ?ྂ ?ྃ ?྄ ?྅)
    (;; misc
     ?ༀ ?༁ ?༂ ?༃ ?༄ ?༅ ?༆ ?༇
     ?༈ ?༉ ?༊ ?་ ?༌ ?། ?༎ ?༏ 
     ?༐ ?༑ ?༒ ?༓ ?༔ ?༕ ?༖ ?༗
     ?༘ ?༙ ?༚ ?༛ ?༜ ?༝ ?༞ ?༟
     ?༠ ?༡ ?༢ ?༣ ?༤ ?༥ ?༦ ?༧
     ?༨ ?༩ ?༪ ?༫ ?༬ ?༭ ?༮ ?༯
     ?༰ ?༱ ?༲ ?༳ ?༴ ?༵ ?༶ ?༷
     ?༸ ?༹ ?༺ ?༻ ?༼ ?༽ ?༾ ?༿)))

(defvar tibetan-consonant-prep '(?ག ?ད ?བ ?མ ?འ))
(defvar tibetan-consonant-head '(?ར ?ལ ?ས))
(defvar tibetan-consonant-foot '(?ཡ ?ར ?ལ ?ཝ ?འ))
(defvar tibetan-consonant-post '(?ག ?ང ?ཌ ?ཎ ?ད ?ན ?བ ?མ ?འ ?ར ?ལ ?ས))
(defvar tibetan-consonant-post2 '(?ད ?ས))
(defun tibetan-consonant-to-subjoined (char)
  (if (and char (<= #x0f40 char) (<= char #xf69)) (+ char #x50) char))
(defun tibetan-subjoined-to-consonant (char)
  (if (and char (<= #x0f90 char) (<= char #xfb9)) (- char #x50) char))

(defvar tibetan-syllable-regexp
  "\\(?:\\([གདབམའ]?[ཀ-ཪ]\\)\\([ྐ-ྐྵ]*\\)\\([ཱ-ཱྀ]?\\)\\([ཾཿྂ-྅]?[ཀ-ཪ]*\\)\\)\\|\\([ༀ-༿]+\\)"
  "Tibetan syllable matching regexp.
1st match .. consonants
2nd match .. subjoins
3rd match .. vowel
4th match .. modifier & consonants2
5th match .. misc")

(defvar tibetan-consonants-to-subjoined-alist
  '(
    ("སཙ" . "སྩ") ("སཀཡ" . "སྐྱ") ("སགཡ" . "སྒྱ")
    ("སཔཡ" . "སྤྱ") ("སབཡ" . "སྦྱ") ("སམཡ" . "སྨྱ")
    ("སཀར" . "སྐྲ") ("སགར" . "སྒྲ") ("སནར" . "སྣྲ")
    ("སཔར" . "སྤྲ") ("སབར" . "སྦྲ") ("སམར" . "སྨྲ")
    ("ཇར" . "ཇྲ") ("ཛར" . "ཛྲ")
    ("རཙཝ" . "རྩྭ") ("རཀཡ" . "རྐྱ") ("རགཡ" . "རྒྱ")
    ("རམཡ" . "རྨྱ")("རགཝ" . "རྒྭ")
    ("གརཝ" . "གྲྭ") ("ཕཡཝ" . "ཕྱྭ")
    ("ཀཝ" . "ཀྭ") ("ཁཝ" . "ཁྭ") ("གཝ" . "གྭ")
    ("ཅཝ" . "ཅྭ") ("ཉཝ" . "ཉྭ") ("ཏཝ" . "ཏྭ")
    ("དཝ" . "དྭ") ("ཙཝ" . "ཙྭ") ("ཚཝ" . "ཚྭ")
    ("ཞཝ" . "ཞྭ") ("ཟཝ" . "ཟྭ") ("རཝ" . "རྭ")
    ("ལཝ" . "ལྭ") ("ཤཝ" . "ཤྭ") ("སཝ" . "སྭ")
    ("ཧཝ" . "ཧྭ") ("ཀཡ" . "ཀྱ") ("ཁཡ" . "ཁྱ")
    ("གཡ" . "གྱ") ("པཡ" . "པྱ") ("ཕཡ" . "ཕྱ")
    ("བཡ" . "བྱ") ("མཡ" . "མྱ")
    ("ཀར" . "ཀྲ") ("ཁར" . "ཁྲ") ("གར" . "གྲ")
    ("ཏར" . "ཏྲ") ("ཐར" . "ཐྲ") ("དར" . "དྲ")
    ("པར" . "པྲ") ("ཕར" . "ཕྲ") ("བར" . "བྲ")
    ("མར" . "མྲ") ("ཤར" . "ཤྲ") ("སར" . "སྲ")
    ("ཧར" . "ཧྲ") ("ཀལ" . "ཀླ")
    ("གལ" . "གླ") ("བལ" . "བླ") ("ཟལ" . "ཟླ")
    ("རལ" . "རླ") ("སལ" . "སླ") ("རཀ" . "རྐ")
    ("རག" . "རྒ") ("རང" . "རྔ") ("རཇ" . "རྗ")
    ("རཉ" . "རྙ") ("རཏ" . "རྟ") ("རད" . "རྡ")
    ("རན" . "རྣ") ("རབ" . "རྦ") ("རམ" . "རྨ")
    ("རཙ" . "རྩ") ("རཛ" . "རྫ")
    ("ལཀ" . "ལྐ") ("ལག" . "ལྒ")
    ("ལང" . "ལྔ") ("ལཅ" . "ལྕ") ("ལཇ" . "ལྗ")
    ("ལཏ" . "ལྟ") ("ལད" . "ལྡ") ("ལཔ" . "ལྤ")
    ("ལབ" . "ལྦ") ("ལཧ" . "ལྷ") ("སཀ" . "སྐ")
    ("སག" . "སྒ") ("སང" . "སྔ") ("སཉ" . "སྙ")
    ("སཏ" . "སྟ") ("སད" . "སྡ") ("སན" . "སྣ")
    ("སཔ" . "སྤ") ("སབ" . "སྦ") ("སམ" . "སྨ")
    )
  "Assoc. list of sequence of pre-vowel full-consonants to
consonant with subjoined consonants." )

;; When Tibetan syllable satisfies the following coditions,
;; (1) vowel is `a', (2) no subjoined consonants or vowel modifier, 
;; (3) more than two consonants, 
;; then alternative transliteration may be possible.  For example, 
;; `བང' may be `bang' or `bnga', or `དགས' may be `dags' or `dgas'.
(defvar tibetan-alternative-syllable-regexp
  "^\\([གདབམའ]\\)\\([གངཌཎདནབམའརལས][དས]?\\)$")

;;; Tibetan Transliteration

;; Basic Transliteration Functions
(defun tibetan-transliteration-table-setup (trans-table encode-table decode-table)
  (let ((chars (apply 'append tibetan-characters))
        (trans (apply 'append trans-table)))
    (while chars
      (and (car chars) (car trans)
           (puthash (car chars) (car trans) encode-table)
           (puthash (car trans) (car chars) decode-table))
      (setq chars (cdr chars) trans (cdr trans)))))

;; Encoding Transliteration
(defun tibetan-transliteration-encode-syllable (syllable encode-table analyzed-p)
  "Encode SYLLABLE in accordance with ENCODE-TABLE.
If ANALYZED-P is t, then matching against `tibetan-syllable-regexp' is 
already done and match-data is already stored."
  (if (not analyzed-p) (string-match tibetan-syllable-regexp syllable))
  (let* ((l            (lambda (char) (or (gethash char encode-table)
                                          (char-to-string char))))
         (a-vowel      (gethash ?ཨ encode-table))
         (consonants   (mapconcat l (string-to-list (match-string 1)) ""))
         (subjoins     (mapconcat l 
                                  (mapcar 'tibetan-subjoined-to-consonant
                                          (string-to-list (match-string 2)))
                                  ""))
         (vowel        (if (and (eq 0 (length (match-string 3)))
                                (eq 0 (length (match-string 5)))) a-vowel
                         (mapconcat l (string-to-list (match-string 3)) "")))
         (mod-cons2    (mapconcat l (string-to-list (match-string 4)) ""))
         (misc         (mapconcat l (string-to-list (match-string 5)) ""))
         (enc-syllable (concat consonants subjoins vowel mod-cons2 misc))
         alt-syllable)
    (when (string-match tibetan-alternative-syllable-regexp syllable)
      (setq alt-syllable 
            (concat (mapconcat l (match-string 1 syllable) "")
                    a-vowel
                    (mapconcat l (match-string 2 syllable) ""))))
    (if alt-syllable (list alt-syllable enc-syllable) (list enc-syllable))))

(defun tibetan-transliteration-encode-region (from to encode-table)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      ;; `re-search-forward' sometimes do now work for Sanskrit "Ri" vowel.
      (while (posix-search-forward tibetan-syllable-regexp nil t)
        (let ((encoded-syllables 
               (save-match-data
                 (tibetan-transliteration-encode-syllable (match-string 0)
                                                          encode-table t))))
          (replace-match
           (if (= (length encoded-syllables) 2)
               (concat (elt encoded-syllables 0) " (" (elt encoded-syllables 1) ")")
             (elt encoded-syllables 0))))))))

;; Decoding Transliteration
(defun tibetan-transliteration-regexp (trans-table encode-table)
  (let* ((l (lambda (char) (gethash char encode-table)))
         (p (regexp-opt (mapcar l tibetan-consonant-prep)))
         (h (regexp-opt (mapcar l tibetan-consonant-head)))
         (c (regexp-opt (elt trans-table 0)))
         (f (regexp-opt (mapcar l tibetan-consonant-foot)))
         (v (regexp-opt (remove nil (append (elt trans-table 1)
                                            (elt trans-table 2)))))
         (m (regexp-opt (elt trans-table 3)))
         (i (regexp-opt (remove nil (elt trans-table 4)))))
    ;; explicit foot-consonant may be denoted by `+'. (e.g. `sh' vs `s+h').
    (concat "\\(?:-?"       ;; group 1 .. (CC)C(C)(V)(M) (1-6)
               "\\(?:\\(?3:" c "\\)\\|"
                  "\\(\\(?2:" h "\\)?\\(?3:" c "\\)\\)\\|"
                  "\\(\\(?1:" p "\\)?\\(?3:" c "\\)\\)\\|"
                  "\\(\\(?1:" p "\\)?\\(?2:" h "\\)?\\(?3:" c "\\)\\)\\)"
               "\\+?\\(?4:" f "\\)?\\(?5:" v "\\)?\\(?6:" m "\\)?\\)\\|"
            "\\(?:-?"    ;; group 2 .. V(M)  (7-8)
               "\\(?7:" v "\\)\\(?8:" m "\\)?\\)\\|"
            "\\(?9:"   ;; group 3 .. misc. (9)
               i "\\)")))

(defun tibetan-transliteration-decode-region (from to regexp table)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let ((case-fold-search nil))
      (while (posix-search-forward regexp nil t)
        (let* ((p (gethash (match-string 1) table))
               (h (gethash (match-string 2) table))
               (c (gethash (match-string 3) table))
               (f (gethash (match-string 4) table))
               (v (gethash (match-string 5) table))
               (m (gethash (match-string 6) table))
               (V (gethash (match-string 7) table))
               (M (gethash (match-string 8) table))
               (i (gethash (match-string 9) table))
               (C (concat (remove nil (list p h c))))
               (C (cdr (assoc C tibetan-consonants-to-subjoined-alist))))
          (when (null C)
            (if h (setq c (tibetan-consonant-to-subjoined c)))
            (setq C (concat (remove nil (list p h c)))))
          (if f (setq f (tibetan-consonant-to-subjoined f)))
          (if (eq v ?ཨ) (setq v nil))
          (if V (setq c ?ཨ))
          (replace-match (concat C (remove nil (list f v m V M i))))))))))

(defun tibetan-process-string (string region-function)
  (with-temp-buffer
    (insert string)
    (funcall region-function (point-min) (point-max))
    (buffer-string)))

;; Transliteration Tables

;; Extended Wylie Transliteration
;; cf.
;; http://tibet.que.ne.jp/misc/EWylie1.html
;; http://www.bibliotheque-dhagpo-kagyu.org/pdfcatal/wylie/ewts.pdf, 
(defvar tibetan-extended-wylie-table
  '(
    (;; consonants 
     "k" "kh" "g" "ng"          ;; U+0F40-U+0F42, U+0F44
     "c" "ch" "j" "ny"          ;; U+0F45-U+0F47, U+0F49
     "T" "Th" "D" "N"           ;; U+0F4A-U+0F4C, U+0F4E
     "t" "th" "d" "n"           ;; U+0F4F-U+0F51, U+0F53
     "p" "ph" "b" "m"           ;; U+0F54-U+0F56, U+0F58
     "ts" "tsh" "dz" "w"        ;; U+0F59-U+0F5B, U+0F5D
     "zh" "z" "'" "y" "r" "l"   ;; U+0F5E-U+0F63
     "sh" "SH" "s" "h")         ;; U+0F64-U+0F67
    (;; inherent vowel
     "a")                       ;; U+0F68
    (;; vowels
     "A" "i" "I" "u" "U"        ;; U+0F71-U+0F75
     "Ri" "RI" "Li" "LI"        ;; U+0F76-U+0F79
     "e" "ai" "o" "au"          ;; U+0F7A-U+0F7D
     "-i" "-I")                 ;; U+0F80-U+0F81
    (;; vmodifier
     "M" "H" "~^" "~" "?" "&")  ;; U+0F7E-U+0F7F U+0F82-U+0F85
    (;; misc
     nil nil nil nil "@" "#" "$" "%"   ;; U+0F00-U+0F07
     "!" nil nil " " "*" "/" "//" "\;" ;; U+0F08-U+0F0F
     "[" "|" "]" "`" ":" nil nil nil   ;; U+0F10-U+0F17
     nil nil nil nil nil nil nil nil   ;; U+0F18-U+0F1F
     "0" "1" "2" "3" "4" "5" "6" "7"   ;; U+0F20-U+0F27
     "8" "9" nil nil nil nil nil nil   ;; U+0F28-U+0F2F
     nil nil nil nil "=" nil nil nil   ;; U+0F30-U+0F37
     nil nil "<" ">" "(" ")" "{" "}")  ;; U+0F38-U+0F3F
    )
  "Extended Wylie Transcription.")

(defvar tibetan-extended-wylie-encode-table (make-hash-table :test 'equal))
(defvar tibetan-extended-wylie-decode-table (make-hash-table :test 'equal))
(tibetan-transliteration-table-setup tibetan-extended-wylie-table
                                    tibetan-extended-wylie-encode-table
                                    tibetan-extended-wylie-decode-table)
(defvar tibetan-extended-wylie-regexp
  (tibetan-transliteration-regexp tibetan-extended-wylie-table
                                  tibetan-extended-wylie-encode-table))
(defun tibetan-extended-wylie-encode-region (from to)
  (interactive "r")
  (tibetan-transliteration-encode-region 
   from to tibetan-extended-wylie-encode-table))
(defun tibetan-extended-wylie-encode-string (str)
  (tibetan-process-string str #'tibetan-extended-wylie-encode-region))
(defun tibetan-extended-wylie-decode-region (from to)
  (interactive "r")
  (tibetan-transliteration-decode-region 
   from to 
   tibetan-extended-wylie-regexp
   tibetan-extended-wylie-decode-table))
(defun tibetan-extended-wylie-decode-string (str)
  (tibetan-process-string str #'tibetan-extended-wylie-decode-region))

;; ACIP Transliteration
;; cf.
;; http://www.asianclassics.org/

(defvar tibetan-acip-table
  '(
    (;; consonants 
     "K" "KH" "G" "NG"          ;; U+0F40-U+0F42, U+0F44
     "C" "CH" "J" "NY"          ;; U+0F45-U+0F47, U+0F49
     "t" "tH" "d" "n"           ;; U+0F4A-U+0F4C, U+0F4E
     "T" "TH" "D" "N"           ;; U+0F4F-U+0F51, U+0F53
     "P" "PH" "B" "M"           ;; U+0F54-U+0F56, U+0F58
     "TZ" "TS" "DZ" "V"         ;; U+0F59-U+0F5B, U+0F5D
     "ZH" "Z" "'" "Y" "R" "L"   ;; U+0F5E-U+0F63
     "SH" "s" "S" "H" )         ;; U+0F64-U+0F67
    (;; inherent vowel
     "A")                       ;; U+0F68
    (;; vowels
     "a" "I" "'I" "U" "u"       ;; U+0F71-U+0F75
     "Ri" nil "Li" nil
     "E" "AI" "O" "AU"          ;; U+0F7A-U+0F7D
     "-I" "-i")                 ;; U+0F80-U+0F81
    (;; vmodifier
     "m" "HA'" "~^" "~" "?" "&" ;; U+0F7E-U+0F7F U+0F82-U+0F85
     )
    (;; misc
     nil nil nil nil "@" "#" "$" "%"   ;; U+0F00-U+0F07
     "!" nil nil " " "*" "," "/" "\;"  ;; U+0F08-U+0F0F
     "[" "|" "]" "`" ":" nil nil nil   ;; U+0F10-U+0F17
     nil nil nil nil nil nil nil nil   ;; U+0F18-U+0F1F
     "0" "1" "2" "3" "4" "5" "6" "7"   ;; U+0F20-U+0F27
     "8" "9" nil nil nil nil nil nil   ;; U+0F28-U+0F2F
     nil nil nil nil "=" nil nil nil   ;; U+0F30-U+0F37
     nil nil "<" ">" "(" ")" "{" "}")  ;; U+0F38-U+0F3F
    )
  "ACPI Transcription")
(defvar tibetan-acip-encode-table (make-hash-table :test 'equal))
(defvar tibetan-acip-decode-table (make-hash-table :test 'equal))
(tibetan-transliteration-table-setup tibetan-acip-table
                                    tibetan-acip-encode-table
                                    tibetan-acip-decode-table)
(defvar tibetan-acip-regexp
  (tibetan-transliteration-regexp tibetan-acip-table
                                  tibetan-acip-encode-table))
(defun tibetan-acip-encode-region (from to)
  (interactive "r")
  (tibetan-transliteration-encode-region from to tibetan-acip-encode-table))
(defun tibetan-acip-encode-string (str)
  (tibetan-process-string str #'tibetan-acip-encode-region))
(defun tibetan-acip-decode-region (from to)
  (interactive "r")
  (tibetan-transliteration-decode-region 
   from to 
   tibetan-acip-regexp
   tibetan-acip-decode-table))
(defun tibetan-acip-decode-string (str)
  (tibetan-process-string str #'tibetan-acip-decode-region))

(provide 'tibet-util+)

;; tibet-util+ ends here.
