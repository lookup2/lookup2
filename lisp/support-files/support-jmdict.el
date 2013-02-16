;;; support-jmdict.el --- support file for "JMDict" file.
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

;; This support-file will search the JMdict file distributed by
;; Jim Breen.  You may need to make the suffix array index
;; by "mksary" program.  (-l option should be attached.)
;;
;; Download site:
;; http://www.csse.monash.edu.au/~jwb/jmdict.html
;;
;; Sample Index Point Generator for JMdict.  If you do not need
;; specific language support, remoeve specific line from the following
;; program.  If you want to handle each langauge differently, then
;; `hard-link' copy the JMdict file for each languages and create
;; language-specific suffix array indexes individually.
;;
;; #!/usr/bin/env ruby -Ku
;; # Usage: ruby jmdict.rb JMdict
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if ((line =~ /^(.*)(<keb>)(.+)<\/keb>/ ) ||
;;       (line =~ /^(.*)(<reb[^>]*>)(.+)<\/reb>/ ) ||
;;       (line =~ /^(.*)(<ent_seq[^>]*>)(.+)<\/ent_seq>/ ) ||
;;       (line =~ /^(.*)(<gloss[^>]*>)(.+)<\/gloss>/ ))
;;     offs = $offset+$1.length
;;     print [offs].pack("N")
;;     offs = offs+$2.length
;;     chars=$3.split(//)
;;     chars.each {|char| 
;;       print [offs].pack("N")
;;       offs = offs+char.length
;;     }
;;   end
;;   $offset+=line.length
;; }

;;; Code:

;;; Customizable variables

(defvar support-jmdict-entry-tags-list
  '(("<gloss xml:lang=\"rus\">" . "</gloss>")
    ("<gloss xml:lang=\"ger\">" . "</gloss>")
    ("<gloss xml:lang=\"fre\">" . "</gloss>")
    ("<gloss>" . "</gloss>")
    ("<reb>" . "</reb>")
    ("<keb>" . "</keb>"))
  "Tags to be searched.  You may edit the variables to reduce the
searching time.")

(defvar support-jmdict-replace-tags
  '(("<gloss xml:lang=\"rus\">" . "ロシア語：")
    ("<gloss xml:lang=\"ger\">" . "ドイツ語：")
    ("<gloss xml:lang=\"fre\">" . "フランス語：")
    ("<gloss>" . "英語：")
    ("<reb>" . "かな：")
    ("<keb>" . "漢字：")
    ("<bib_txt>" . "書誌情報：")
    ("<etym>" . "語源：")
    ("<ent_seq>" . "語彙番号：")
    ("<ke_inf>" . "漢字情報コード：")
    ("<ke_pri>" . "漢字重要度：")
    ("<dial>" . "方言：")
    ("<example>" . "用例：")))


(defvar support-jmdict-replace-tags-regexp
  (regexp-opt
   (mapcar 'car support-jmdict-replace-tags)))

(defvar support-jmdict-replace-entities
  '(
    ("MA" . "martial arts term")
    ("X" . "rude or X-rated term (not displayed in educational software)")
    ("abbr" . "abbreviation")
    ("adj-i" . "adjective (keiyoushi)")
    ("adj-na" . "adjectival nouns or quasi-adjectives (keiyodoshi)")
    ("adj-no" . "nouns which may take the genitive case particle `no'")
    ("adj-pn" . "pre-noun adjectival (rentaishi)")
    ("adj-t" . "`taru' adjective")
    ("adj-f" . "noun or verb acting prenominally")
    ("adj" . "former adjective classification (being removed)")
    ("adv" . "adverb (fukushi)")
    ("adv-to" . "adverb taking the `to' particle")
    ("arch" . "archaism")
    ("ateji" . "ateji (phonetic) reading")
    ("aux" . "auxiliary")
    ("aux-v" . "auxiliary verb")
    ("aux-adj" . "auxiliary adjective")
    ("Buddh" . "Buddhist term")
    ("chem" . "chemistry term")
    ("chn" . "children's language")
    ("col" . "colloquialism")
    ("comp" . "computer terminology")
    ("conj" . "conjunction")
    ("ctr" . "counter")
    ("derog" . "derogatory")
    ("eK" . "exclusively kanji")
    ("ek" . "exclusively kana")
    ("exp" . "Expressions (phrases, clauses, etc.)")
    ("fam" . "familiar language")
    ("fem" . "female term or language")
    ("food" . "food term")
    ("geom" . "geometry term")
    ("gikun" . "gikun (meaning) reading")
    ("hon" . "honorific or respectful (sonkeigo) language")
    ("hum" . "humble (kenjougo) language")
    ("iK" . "word containing irregular kanji usage")
    ("id" . "idiomatic expression")
    ("ik" . "word containing irregular kana usage")
    ("int" . "interjection (kandoushi)")
    ("io" . "irregular okurigana usage")
    ("iv" . "irregular verb")
    ("ling" . "linguistics terminology")
    ("m-sl" . "manga slang")
    ("male" . "male term or language")
    ("male-sl" . "male slang")
    ("math" . "mathematics")
    ("mil" . "military")
    ("n" . "noun (common) (futsuumeishi)")
    ("n-adv" . "adverbial noun (fukushitekimeishi)")
    ("n-suf" . "noun, used as a suffix")
    ("n-pref" . "noun, used as a prefix")
    ("n-t" . "noun (temporal) (jisoumeishi)")
    ("num" . "numeric")
    ("oK" . "word containing out-dated kanji")
    ("obs" . "obsolete term")
    ("obsc" . "obscure term")
    ("ok" . "out-dated or obsolete kana usage")
    ("on-mim" . "onomatopoeic or mimetic word")
    ("pn" . "pronoun")
    ("poet" . "poetical term")
    ("pol" . "polite (teineigo) language")
    ("pref" . "prefix")
    ("prt" . "particle")
    ("physics" . "physics terminology")
    ("rare" . "rare")
    ("sens" . "sensitive")
    ("sl" . "slang")
    ("suf" . "suffix")
    ("uK" . "word usually written using kanji alone")
    ("uk" . "word usually written using kana alone")
    ("v1" . "Ichidan verb")
    ("v2a-s" . "Nidan verb with 'u' ending (archaic)")
    ("v4h" . "Yondan verb with `hu/fu' ending (archaic)")
    ("v4r" . "Yondan verb with `ru' ending (archaic)")
    ("v5" . "Godan verb (not completely classified)")
    ("v5aru" . "Godan verb - -aru special class")
    ("v5b" . "Godan verb with `bu' ending")
    ("v5g" . "Godan verb with `gu' ending")
    ("v5k" . "Godan verb with `ku' ending")
    ("v5k-s" . "Godan verb - Iku/Yuku special class")
    ("v5m" . "Godan verb with `mu' ending")
    ("v5n" . "Godan verb with `nu' ending")
    ("v5r" . "Godan verb with `ru' ending")
    ("v5r-i" . "Godan verb with `ru' ending (irregular verb)")
    ("v5s" . "Godan verb with `su' ending")
    ("v5t" . "Godan verb with `tsu' ending")
    ("v5u" . "Godan verb with `u' ending")
    ("v5u-s" . "Godan verb with `u' ending (special class)")
    ("v5uru" . "Godan verb - Uru old class verb (old form of Eru)")
    ("v5z" . "Godan verb with `zu' ending")
    ("vz" . "Ichidan verb - zuru verb (alternative form of -jiru verbs)")
    ("vi" . "intransitive verb")
    ("vk" . "Kuru verb - special class")
    ("vn" . "irregular nu verb")
    ("vr" . "irregular ru verb, plain form ends with -ri")
    ("vs" . "noun or participle which takes the aux. verb suru")
    ("vs-s" . "suru verb - special class")
    ("vs-i" . "suru verb - irregular")
    ("kyb" . "Kyoto-ben")
    ("osb" . "Osaka-ben")
    ("ksb" . "Kansai-ben")
    ("ktb" . "Kantou-ben")
    ("tsb" . "Tosa-ben")
    ("thb" . "Touhoku-ben")
    ("tsug" . "Tsugaru-ben")
    ("kyu" . "Kyuushuu-ben")
    ("rkb" . "Ryuukyuu-ben")
    ("vt" . "transitive verb")
    ("vulg" . "vulgar expression or word")))

(defvar support-jmdict-replace-entities-regexp
  (concat "&\\("
          (regexp-opt
           (mapcar 'car support-jmdict-replace-entities))
          "\\);"))

(defun support-jmdict-arrange-structure (entry)
  "Arrange Structure of ENTRY."
  (goto-char (point-min))
  (while (re-search-forward support-jmdict-replace-tags-regexp nil t)
    (replace-match
     (cdr (assoc (match-string 0) support-jmdict-replace-tags))))
  (goto-char (point-min))
  (while (re-search-forward support-jmdict-replace-entities-regexp nil t)
    (replace-match
     (cdr (assoc (match-string 1) support-jmdict-replace-entities))))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (if (looking-at "\n") (delete-region (point-min) (1+ (point-min))))
  (goto-char (point-min))
  (while (re-search-forward "\\([ 	]*\n\\)+" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (re-search-forward "語彙番号：[0-9]+" nil t)
    (lookup-make-region-heading (match-beginning 0) (match-end 0) 1))
  )

(defun support-jmdict-head-tags (content)
  (if (or (string-match "<[kr]eb>\\(.+?\\)</[kr]eb>" content)
          (string-match "<ent_seq>\\(.+?\\)</ent_seq>" content))
      (match-string 1 content)
    (if (< 2 (length content))
        (error "jmdict: improper content! %s" content))))
    
(setq lookup-support-options
      (list :title "JMDict"
            :arranges '((reference support-jmdict-arrange-structure))
            :entry-tags-list support-jmdict-entry-tags-list
            :content-tags '("<entry>" . "</entry>")
            :head-tags 'support-jmdict-head-tags
            :code-tags '("<ent_seq>" . "</ent_seq>")
            :coding 'utf-8
            )) ;; <keb>..</keb> or <reb>..</reb>

;;; support-jmdict.el ends here
