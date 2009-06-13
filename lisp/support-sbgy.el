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

(require 'lookup)

(defvar support-sbgy-voice-brackets
  '(("〖" . "〗") ;; 平声
    ("「" . "】") ;; 上声
    ("【" . "】") ;; 上声
    ("【" . "」"))) ;; 入声

(defvar support-sbgy-consonants
  '("p" "pʰ" "bʰ" "m" "t" "tʰ" "dʰ" "n" "ţ" "ţʰ" "ɖʰ" "ɳ" "ts" "tsʰ" "dzʰ" "s" "z" "ʧ" "ʧʰ" "dʒʰ" "ʃ" "dʐʰ" "tɕ" "tɕʰ" "dʑʰ" "ɕ" "ʑ" "k" "kʰ" "gʰ" "ŋ" "ʔ" "x" "ɣ" "" "j" "l" "nʑ" ))

(defvar support-sbgy-vowels
  '("uŋ" "ĭuŋ" "uk" "ĭuk" "uoŋ" "uok" "ĭwoŋ" "ĭwok" "ɔŋ" "ɔk"
    "ĭe" "ĭwe" "i" "wi" "ĭə" "ĭəi" "ĭwəi" "ĭo" "ĭu" "u" "iei" "iwei"
    "ĭɛi" "ĭwɛi" "ɑi" "uɑi" "ai" "wai" "ɐi" "wɐi" "æi" "wæi" "uɒi"
    "ɒi" "ĭɐi" "ĭwɐi" "ĭĕn" "ĭwĕn" "ĭĕt" "ĭwĕt" "ĭuĕn" "ĭuĕt" "ĭen"
    "ĭet" "ĭuən" "ĭuət" "ĭən" "ĭət" "uən" "uət" "ən" "ət" "ĭɐn" "ĭwɐn"
    "ĭɐt" "ĭwɐt" "ɑn" "ɑt" "uɑn" "uɑt" "an" "wan" "at" "wat" "æn"
    "wæn" "æt" "wæt" "ien" "iwen" "iet" "iwet" "ĭɛn" "ĭwɛn" "ĭɛt"
    "ĭwɛt" "ieu" "ĭɛu" "au" "ɑu" "ɑ" "uɑ" "ĭɑ" "ĭuɑ" "a" "wa" "ĭa"
    "ĭaŋ" "ĭwaŋ" "ĭak" "ĭwak" "ɑŋ" "uɑŋ" "ɑk" "uɑk" "ɐŋ" "wɐŋ"
    "ĭɐŋ" "ĭwɐŋ" "ɐk" "wɐk" "ĭɐk" "ĭwɐk" "æŋ" "wæŋ" "æk" "wæk"
    "ĭɛŋ" "ĭwɛŋ" "ĭɛk" "ĭwɛk" "ieŋ" "iweŋ" "iek" "iwek" "ĭəŋ"
    "ĭək" "ĭwək" "əŋ" "uəŋ" "ək" "uək" "ĭəu" "əu" "iəu" "ĭĕm" "ĭĕp"
    "ɒm" "ɒp" "ɑm" "ɑp" "ĭɛm" "ĭɛp" "iem" "iep" "ɐm" "ɐp" "am" "ap"
    "ĭɐm" "ĭɐp" "ĭwɐm" "ĭwɐp"))

(defvar support-sbgy-pronunciation-regexp
  (concat "^" (regexp-opt support-sbgy-consonants)
          (regexp-opt support-sbgy-vowels) "?[˥˩]*$"))

(defun support-sbgy-arrange-structure (entry)
  "Arrange contents of ENTRY."
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
  (re-search-forward "[㐀-鿿𠀀-𯟿]" nil t)
  (goto-char (point-min))
  (insert (match-string 0) "\n"))

(defun support-sbgy-entry-tags-list (string method)
  (if (and (equal method 'text)
           (lookup-text-cjk-p string))
      '(("" . ""))
    (cond ((lookup-text-single-cjk-p string)
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

(defun support-sbgy-head-tags (x)
  (let* ((ipa (ndsary-extract-string x "ipa=\"" "\""))
         (yomi (ndsary-extract-string x "onyomi=\"" "\""))
         (char (ndsary-extract-string x ">" "<note"))
         (brckts 
          (cond ((string-match "˩$" ipa) 0)
                ((string-match "˩˥$" ipa) 2)
                ((string-match "˥$" ipa) 1)
                (t 4))))
    (format "%s%s%s%s《%s》" (car (elt support-sbgy-voice-brackets brckts))
                            char 
                            (cdr (elt support-sbgy-voice-brackets brckts))
                            yomi ipa)))

(setq lookup-support-options
      (list :title "宋本廣韻"
            :entry-tags-list 'support-sbgy-entry-tags-list
            :content-tags '("<voice_part" . "</voice_part>")
            :code-tags '("id=\"" . "\">")
            :head-tags 'support-sbgy-head-tags
            :arranges '((structure support-sbgy-arrange-structure))))

;;; support-sbgy.el ends here
