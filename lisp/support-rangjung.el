;;; support-rangjung.el --- support file for "Rangjung Yeshe" file.
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

;; This support-file will search the Rangjung Yeshe Tibetan-English
;; Dictionary, distributed by Tibetan-Himalayan Library.
;;
;; You need to make the suffix array index by "mksary" program.  
;; Following script creates index points at only headwords.
;;
;; #!/usr/bin/env ruby -Ku
;; # Usage: ruby rangjung.rb ry-dic2003.txt
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if line =~ /^(.*?)( - .+)$/
;;     rest=$2
;;     chars=$1.split(//)
;;     offs = $offset
;;     chars.each {|char| 
;;       print [offs].pack("N")
;;       offs = offs+char.length
;;     }
;;     print [offs+rest.length].pack("N")
;;   end
;;   $offset+=line.length
;; }

;; This dictionary can be purchased from the following site:
;; http://www.rangjung.com/ry-dic.htm

;; TODO: AA(ཨ) と、A(འ) の処理
;;; Code:

(require 'lookup)
(require 'ucs-normalize)

;;; Transliteration data
(defvar tibetan-characters
  '(
    (;;consonants
     ?ཀ ?ཁ ?ག ?གྷ ?ང
     ?ཅ ?ཆ ?ཇ ?ཉ
     ?ཊ ?ཋ ?ཌ ?ཌྷ ?ཎ
     ?ཏ ?ཐ ?ད ?དྷ ?ན
     ?པ ?ཕ ?བ ?བྷ ?མ
     ?ཙ ?ཚ ?ཛ ?ཛྷ ?ཝ
     ?ཞ ?ཟ ?འ ?ཡ ?ར ?ལ
     ?ཤ ?ཥ ?ས ?ཧ ?ཨ ?ཀྵ ?ཪ)
    (;; vowels
     ?ཱ ?ི ?ཱི ?ུ ?ཱུ
     ?ྲྀ ?ཷ ?ླྀ ?ཹ 
     ?ེ ?ཻ ?ོ ?ཽ
     ?ྀ ?ཱྀ
     nil) ;; dummy slot for inherent vowel "a"
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
(defun tibetan-consonant-to-subjoined (char)
  (if (and char (<= #x0f40 char) (<= char #xf69)) (+ char #x50) char))
(defun tibetan-subjoined-to-consonant (char)
  (if (and char (<= #x0f90 char) (<= char #xfb9)) (- char #x50) char))
(defvar tibetan-syllable-regexp
  "\\(?:\\([གདབམའ]?[ཀ-ཪ]\\)\\([ྐ-ྐྵ]*\\)\\([ཱ-ཱྀ]?\\)\\([ཾཿྂ-྅]?[ཀ-ཪ]*\\)\\)\\|\\([ༀ-༿]+\\)")

(defvar tibetan-extended-wylie
  '(
    (;; consonants 
     "k" "kh" "g" "g+h" "ng"    ;; U+0F40-U+0F44
     "c" "ch" "j" "ny"          ;; U+0F45-U+0F47, U+0F49
     "T" "Th" "D" "D+h" "N"     ;; U+0F4A-U+0F4E
     "t" "th" "d" "d+h" "n"     ;; U+0F4F-U+0F53
     "p" "ph" "b" "b+h" "m"     ;; U+0F54-U+0F58
     "ts" "tsh" "dz" "dz+h" "w" ;; U+0F59-U+0F5D
     "zh" "z" "'" "y" "r" "l"  ;; U+0F5E-U+0F63
     "sh" "SH" "s" "h" nil nil nil) ;; U+0F64-U+0F6A
    (;; vowels
     "A" "i" "I" "u" "U"        ;; U+0F71-U+0F75
     "Ri" "RI" "Li" "LI"        ;; U+0F76-U+0F79
     "e" "ai" "o" "au"          ;; U+0F7A-U+0F7D
     "-i" "-I"                  ;; U+0F80-U+0F81
     "a")                       ;; non-apparent vowel
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
(let ((chars (apply 'append tibetan-characters))
      (trans (apply 'append tibetan-extended-wylie)))
  (while chars
    (and (car chars) (car trans)
         (puthash (car chars) (car trans) tibetan-extended-wylie-encode-table)
         (puthash (car trans) (car chars) tibetan-extended-wylie-decode-table))
    (setq chars (cdr chars) trans (cdr trans))))
(defun tibetan-extended-wylie-encode-char (char)
  (gethash char tibetan-extended-wylie-encode-table))

(defvar tibetan-extended-wylie-regexp
  (let* ((p (regexp-opt (mapcar 'tibetan-extended-wylie-encode-char
                                tibetan-consonant-prep)))
         (h (regexp-opt (mapcar 'tibetan-extended-wylie-encode-char
                                tibetan-consonant-head)))
         (c (regexp-opt (elt tibetan-extended-wylie 0)))
         (f (regexp-opt (mapcar 'tibetan-extended-wylie-encode-char
                                tibetan-consonant-foot)))
         (v (regexp-opt (elt tibetan-extended-wylie 1)))
         (m (regexp-opt (elt tibetan-extended-wylie 2)))
         (i (regexp-opt (delete nil (elt tibetan-extended-wylie 3)))))
    (concat "\\(?:\\(" p "\\)?\\(" h "\\)?\\(" c "\\)\\(" f "\\)?\\(" v "\\)?"
                 "\\(" m "\\)?\\)\\|\\(" i "\\)")))

(defun tibetan-extended-wylie-decode-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward tibetan-extended-wylie-regexp nil t)
        (let ((p (gethash (match-string 1) tibetan-extended-wylie-decode-table))
              (h (gethash (match-string 2) tibetan-extended-wylie-decode-table))
              (c (gethash (match-string 3) tibetan-extended-wylie-decode-table))
              (f (gethash (match-string 4) tibetan-extended-wylie-decode-table))
              (v (gethash (match-string 5) tibetan-extended-wylie-decode-table))
              (m (gethash (match-string 6) tibetan-extended-wylie-decode-table))
              (i (gethash (match-string 7) tibetan-extended-wylie-decode-table)))
          (if h (setq c (tibetan-consonant-to-subjoined c)))
          (if f (setq f (tibetan-consonant-to-subjoined f)))
          (replace-match (concat (delete nil (list p h c f v m i)))))))))
(defun tibetan-extended-wylie-decode-string (str)
  (with-temp-buffer
    (insert str)
    (tibetan-extended-wylie-decode-region (point-min) (point-max))
    (buffer-string)))

(defun tibetan-extended-wylie-encode-region (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward tibetan-syllable-regexp nil t)
        (replace-match
         (save-match-data
           (concat 
            (mapconcat 'tibetan-extended-wylie-encode-char
                       (string-to-list (match-string 1)) "")
            (mapconcat 'tibetan-extended-wylie-encode-char
                       (mapcar 'tibetan-subjoined-to-consonant
                               (string-to-list (match-string 2))) "")
            (if (and (equal 0 (length (match-string 3)))
                     (equal 0 (length (match-string 5)))) "a"
              (mapconcat 'tibetan-extended-wylie-encode-char
                         (string-to-list (match-string 3)) ""))
            (mapconcat 'tibetan-extended-wylie-encode-char
                       (string-to-list (match-string 4)) "")
            (mapconcat 'tibetan-extended-wylie-encode-char
                       (string-to-list (match-string 5)) ""))))))))
(defun tibetan-extended-wylie-encode-string (str)
  (with-temp-buffer
    (insert str)
    (tibetan-extended-wylie-encode-region (point-min) (point-max))
    (buffer-string)))

;; Query-Filter
(defun support-rangjung-query-filter (query)
  (if (string-match "^[ༀ-࿿]+$" (lookup-query-string query))
      (let ((query (copy-lookup-query query))
            (string 
             (tibetan-extended-wylie-encode-string
              (lookup-query-string query))))
        (setf (lookup-query-string query) string)
        query)
    query))

;;; Arrangements
(defun support-rangjung-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (insert (lookup-entry-heading entry))
  (let ((heading 
         (replace-regexp-in-string " (.+)" "" (lookup-entry-heading entry))))
    (while (re-search-forward (concat "^" (regexp-quote heading) " ") nil t)
      (replace-match "")))
  (goto-char (point-min))
  (while (re-search-forward "{\\(.+?\\)}" nil t)
    (let ((marker (make-marker)))
      (set-marker marker (match-end 0))
      (goto-char (match-beginning 0))
      (insert (tibetan-extended-wylie-decode-string (match-string 1)) " ")
      (goto-char (marker-position marker)))))

(defun support-rangjung-head-tags (content)
  (string-match "^\\(.+?\\) - " content)
  (let ((head (match-string 1 content)))
    (concat head
            " (" 
            (tibetan-extended-wylie-decode-string head) 
            ")")))

(setq lookup-support-options
      (list :title "RangJung"
            :query-filter 'support-rangjung-query-filter
            :entry-tags '("\n" . " - ")
            :content-tags '("\n" . "]\n")
            :arranges '((structure support-rangjung-arrange-structure))
            :head-tags 'support-rangjung-head-tags))

;;; support-rangjung.el ends here
