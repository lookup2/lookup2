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
;;
;; Following Program will make index point file, which then can be
;; sorted by 'mksary -s' command.  

;; #!/usr/bin/env ruby -Ku
;; # Usage: ruby sbgy.rb sbgy.xml
;; #        mksary -s sbgy.xml
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if line =~ /^(.+)(ipa=")([^"]+)"/
;;     print [$offset+$1.length].pack("N")
;;     print [$offset+$1.length+$2.length].pack("N")
;;     offs=$offset+$1.length+$2.length
;;     chars=$3.split(//)
;;     chars.each {|char| 
;;       print [offs].pack("N")
;;       offs = offs+char.length
;;     }
;;   end
;;   if line =~ /^(.+)(onyomi=")([^"]+)"/
;;     print [$offset+$1.length].pack("N")
;;     print [$offset+$1.length+$2.length].pack("N")
;;     offs=$offset+$1.length+$2.length
;;     chars=$3.split(//)
;;     chars.each {|char| 
;;       print [offs].pack("N")
;;       offs = offs+char.length
;;     }
;;   end
;;   if line =~ /^(.+d id="[^"]+")>.+</
;;     print [$offset+$1.length].pack("N")
;;   end
;;   if line =~ /^(.+<original_word)>.+<rewrite_word>/ 
;;     print [$offset+$1.length].pack("N")
;;   end
;;   $offset+=line.length
;; }

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
  (let ((head (progn (re-search-forward "[㐀-鿿𠀀-𯟿]" nil t)
                     (match-string 0))))
    (goto-char (point-min))
    (insert head "\n")
    (if (re-search-forward
         "<voice_part ipa=\"\\([^\"]+\\)\" onyomi=\"\\([^\"]+\\)\">" nil t)
        (replace-match
         (concat "【IPA】" (match-string 1) "【音読み】" (match-string 2))))
    (goto-char (point-min))
    (while (re-search-forward "</voice_part>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\t+" nil t)
      (replace-match ""))))

(defun support-sbgy-entry-pairs (query)
  (let ((string (lookup-query-string query)))
    (cond ((lookup-text-single-cjk-p string)
           '((">" . "<note>")
             (">" . "<added_note>")
             (">" . "<headnote>")
             (">" . "<rewrite_word>")
             (">" . "</rewrite_word>")))
          ((string-match support-sbgy-pronunciation-regexp string)
           '(("ipa=\"" . "\"")))
          ((string-match "^[ア-ン]+$" string)
           '(("onyomi=\"" . "\"")))
          (t nil))))

(defun support-sbgy-entry-func ()
  (let (code heading)
    (message "debug: %s" (buffer-substring (point-min) (point-max)))
    (if (re-search-forward "ipa=\"\\(.*?\\)\"")
        (setq code (match-string 1))
      (if (re-search-forward ">\\(.\\)<")
          (setq heading (match-string 1))))
    (cons code heading)))

(setq lookup-support-options
      (list :title "宋本廣韻"
            :entry-start-end-pairs 'support-sbgy-entry-pairs
            :charsets 'lookup-text-single-cjk-p
            :entry-func 'support-sbgy-entry-func
            :content-start "<voice_part" :content-end "</voice_part>"
            :arranges '((replace support-sbgy-arrange-structure))))

;;; support-sbgy.el ends here
