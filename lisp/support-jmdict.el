;;; support-JMDict.el --- support file for "JMDict" file.
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
;; Sample Index Point Generator for JMdict
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

(require 'lookup)

(defvar support-jmdict-search-tags
  '(("<gloss xml:lang=\"rus\">" . "</gloss>")
    ("<gloss xml:lang=\"ger\">" . "</gloss>")
    ("<gloss xml:lang=\"fre\">" . "</gloss>")
    ("<gloss>" . "</gloss>")
    ("<reb>" . "</reb>")
    ("<keb>" . "</keb>"))
  "Tags to be searched.  You may edit the variables to reduce the
search speed.")

(defvar support-jmdict-replace-tags
  '(("<gloss xml:lang=\"rus\">" . "ロシア語：")
    ("<gloss xml:lang=\"ger\">" . "ドイツ語：")
    ("<gloss xml:lang=\"fre\">" . "フランス語：")
    ("<gloss>" . "英語：")
    ("<reb>" . "かな：")
    ("<keb>" . "漢字：")
    ("<ent_seq>" . "語彙番号：")
    ("<ke_inf>" . "漢字情報コード：")
    ("<ke_pri>" . "漢字重要度：")))

(defun support-jmdict-arrange-structure (entry)
  (let ((tags support-jmdict-replace-tags))
    (while tags
      (goto-char (point-min))
      (while (re-search-forward (caar tags) nil t)
        (replace-match (cdar tags)))
      (setq tags (cdr tags)))
    (goto-char (point-min))
    (while (re-search-forward "<.+?>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (if (looking-at "\n") (delete-region (point-min) (1+ (point-min))))
    (while (re-search-forward "\\([ 	]*\n\\)+" nil t)
      (replace-match "\n"))))

(defun support-jmdict-heading-func (code dictionary)
  (let ((entry-start (lookup-dictionary-option dictionary :entry-start)))
    (cond ((string-match "<gloss>" entry-start)
           (concat "[英語]" code))
          ((string-match "<gloss xml:lang=\"rus\">" entry-start)
           (concat "[ロシア語]" code))
          ((string-match "<gloss xml:lang=\"fre\">" entry-start)
           (concat "[フランス語]" code))
          ((string-match "<gloss xml:lang=\"ger\">" entry-start)
           (concat "[ドイツ語]" code)))))

(setq lookup-support-options
      (list :title "JMDict"
            :arranges '((reference support-jmdict-arrange-structure))
            :entry-start-end-pairs 
            support-jmdict-search-tags
            :content-start "<entry>" :content-end "</entry>"
            :heading 'support-jmdict-heading-func
            :max-hits 100 :regular t))

;;; support-jmdict.el ends here
