;;; support-wikipedia.el --- support for "Wikipedia" Summary XML file.
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

;; This support file provides the ability to instantly search the
;; wikipedia summary file.  You will need to make the suffix array
;; index by "mksary".
;;
;; Unfortunately, current English wikipedia summary file exceeds
;; 2Gbyte, so you must split it in half (by `split' command, for
;; example), to make it less than 2Gbyte to use it.  
;;
;; Download site:
;; http://download.wikipedia.org/enwiki/latest/ (English)
;; http://download.wikipedia.org/jawiki/latest/ (Japanese)
;; 
;; Following Program will make index point file, which then can be
;; sorted by 'mksary -s' command.  (Please make sure that your text
;; does not exeed 2G byte.  In that case, please split the text.)
;;
;; #!/usr/bin/env ruby -Ku
;; # Usage: ruby wiki.rb jawiki-latest-abstract.xml
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if ((line =~ /^(<title>Wikipedia: )(.+)<\/title>/ ) ||
;;       (line =~ /^(<title>Wikipedia:&amp;#32;)(.+)<\/title>/ ) ||
;;       (line =~ /^(<title>Wikipédia&amp;nbsp;:&amp;#32;)(.+)<\/title>/ ))
;;     print [$offset].pack("N")
;;     offs = $offset+$1.length
;;     chars=$2.split(//)
;;     chars.each {|char| 
;;       print [offs].pack("N")
;;       offs = offs+char.length
;;     }
;;   end
;;   $offset+=line.length
;; }

;;; Code:

(require 'lookup)
(require 'lookup-utils)

(defvar support-wikipedia-replace-entities
  '(
    ("amp" . "&")
    ("lt" . "<")
    ("gt" . ">")))

(defvar support-wikipedia-replace-entities-regexp
  (concat "&\\("
          (regexp-opt
           (mapcar 'car support-wikipedia-replace-entities))
          "\\);"))

(defun support-wikipedia-arrange-structure (entry)
  "Attach contents of ENTRY a link and remove tags."
  (goto-char (point-min))
  (while (re-search-forward "<url>\\(.+?\\)</url>" nil t)
    (let ((link (match-string 1))
          (start (match-beginning 0)))
      (replace-match "《→リンク》")
      (lookup-url-set-link start (point) link)))
  (goto-char (point-min))
  (while (re-search-forward
          (concat "<sublink.+?>.*?"
                  "<anchor>\\(.+?\\)</anchor>.*?"
                  "<link>\\(.+?\\)</link></sublink>") nil t)
    (let ((start (match-beginning 0))
          (anchor (match-string 1))
          (link (match-string 2)))
      (replace-match anchor)
      (lookup-url-set-link start (point) link)))
  (goto-char (point-min))
  (while (re-search-forward "</?doc>[\t\n]+" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<title>Wikipedia: " nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<title>Wikipedia:&amp;#32;" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<title>Wikipédia&amp;nbsp;:&amp;#32;" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward support-wikipedia-replace-entities-regexp nil t)
    (replace-match
     (cdr (assoc (match-string 1) support-wikipedia-replace-entities))))
)

(setq lookup-support-options
      (list :title 
            (cond ((string-match "/jawiki" lookup-support-dictionary-id)
                   "Wikipedia 日本語")
                  ((string-match "/enwiki" lookup-support-dictionary-id)
                   "Wikipedia English")
                  ((string-match "/frwiki" lookup-support-dictionary-id)
                   "Wikipédia Française")
                  ((string-match "/zhwiki" lookup-support-dictionary-id)
                   "Wikipedia 中文")
                  (t "Wikipedia"))
            :entry-tags
            (cons (cond ((string-match "/enwiki" lookup-support-dictionary-id)
                         "<title>Wikipedia:&amp;#32;")
                        ((string-match "/frwiki" lookup-support-dictionary-id)
                         "<title>Wikipédia&amp;nbsp;:&amp;#32;")
                        (t "<title>Wikipedia: "))
                  "</title>")
            :content-tags '("<doc>" . "</doc>")
            :arranges '((reference support-wikipedia-arrange-structure))
            :charsets
            (cond ((string-match "/enwiki" lookup-support-dictionary-id)
                   '(ascii))
                  ((string-match "/jawiki" lookup-support-dictionary-id)
                   '(ascii japanese-jisx0208))
                  ((string-match "/frwiki" lookup-support-dictionary-id)
                   '(iso-8859-1))
                  ((string-match "/zhwiki" lookup-support-dictionary-id)
                   '(ascii chinese-gb2312))
                  (t nil))))

;;; support-wikipedia.el ends here
