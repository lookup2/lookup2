;;; support-wikipedia.el --- support for "Wikipedia" Abstract XML file. -*- coding: utf-8 -*-
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

;;;
;;; Customizable variables
;;;
(defvar support-wikipedia-link-notation "《→リンク》")

;;;
;;; Internal Constants
;;;

(defconst support-wikipedia-options-list
  '(("en" 
     (ascii)
     ("<title>Wikipedia:&amp;#32;" . "</title>"))
    ("ja"
     (ascii japanese-jisx0208)
     ("<title>Wikipedia: " . "</title>"))
    ("fr"
     (iso-8859-1)
     ("<title>Wikipédia&amp;nbsp;:&amp;#32;" . "</title>"))
    ("zh"
     (ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2)
     ("<title>Wikipedia: " . "</title>")))
  "Assoc List for `:charsets' and `:entry-tags' for each languages.")

(defconst support-wikipedia-replace-entities
  '(
    ("amp" . "&")
    ("lt" . "<")
    ("gt" . ">")))

(defconst support-wikipedia-replace-entities-regexp
  (concat "&\\("
          (regexp-opt
           (mapcar 'car support-wikipedia-replace-entities))
          "\\);"))

(defun support-wikipedia-dictionary-options (dictionary-id)
  (let* ((lang-id (replace-regexp-in-string 
                   "^.+?/\\(..\\)wiki.+$" "\\1" dictionary-id))
         (options (assoc lang-id support-wikipedia-options-list))
         (charsets (elt options 1))
         (entry-tags (or (elt options 2)
                         '("<title>Wikipedia: " . "</title>"))))
    (list 
     :title (concat "Wikipedia (" lang-id ")")
     :charsets charsets
     :entry-tags entry-tags
     :code-tags entry-tags
     :content-tags '("<doc>" . "</doc>")
     :arranges '((reference support-wikipedia-arrange-reference)))))

(defun support-wikipedia-arrange-reference (entry)
  "Attach contents of ENTRY a link and remove tags."
  (let* ((dictionary (lookup-entry-dictionary entry))
         (entry-tags (or (lookup-dictionary-option dictionary :entry-tags t)
                          support-wikipedia-entry-tags)))
    (goto-char (point-min))
    (while (re-search-forward "<url>\\(.+?\\)</url>" nil t)
      (let ((link (match-string 1))
            (start (match-beginning 0)))
        (replace-match support-wikipedia-link-notation)
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
    (while (re-search-forward (car entry-tags) nil t) (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<.+?>" nil t) (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward support-wikipedia-replace-entities-regexp nil t)
      (replace-match
       (cdr (assoc (match-string 1) support-wikipedia-replace-entities))))))

(setq lookup-support-options
      (support-wikipedia-dictionary-options lookup-support-dictionary-id))

;;; support-wikipedia.el ends here
