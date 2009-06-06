;;; support-swjz.el --- suport file for "説文解字注" -*- coding: utf-8 -*-
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

;; This agent will search the the "説文解字注" XML dictionary file for
;; the head-word character.  The dictionary can be downloaded from the
;; following URL.
;;
;; http://kanji-database.sourceforge.net/dict/swjz/index.html
;;
;; Following Program will make index point file, which then can be
;; sorted by 'mksary -s' command.  
;;
;; #!/usr/bin/env ruby -Ku
;; # Usage: ruby swjz.rb swjz.xml
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if line =~ /^(.*<img src=.+?)(>).+<\/img>/ 
;;     print [$offset+$1.length].pack("N")
;;     #print [$offset+$1.length+$2.length].pack("N")
;;   elsif line =~ /^(.*<wordhead.+?)(>).+<\/wordhead>/ 
;;     print [$offset+$1.length].pack("N")
;;     #print [$offset+$1.length+$2.length].pack("N")
;;   end
;;   if line =~ /^(.*)id=".?/
;;     print [$offset+$1.length].pack("N")
;;   end
;;   $offset+=line.length
;; }

;;; Usage
;;
;;  * agent `location' must contain "swjz" and `:name' must be "char".
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/swjz/all.xml"
;;                   :dict-specs ((:name "char") (:name "word"))))
;;           ....))

;;; Code:

(require 'lookup)

(defun support-swjz-arrange-structure (entry)
  "Attach contents of ENTRY a link and remove tags."
  (goto-char (point-min))
  (while (re-search-forward "<wordhead.+?</wordhead>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 2.0)) face lookup-heading-2-face)))
  (goto-char (point-min))
  (while (re-search-forward "<explanation.+?</explanation>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 1.5)) face lookup-heading-3-face)))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (search-forward "	" nil t) (replace-match ""))
  (goto-char (point-min))
  (if (looking-at "$") (delete-region (point) (1+ (point))))
  )

(setq lookup-support-options
      (list :title "説文解字注"
            :entry-tags-list '((">" . "</wordhead>")
                               (">" . "</img></wordhead>")
                               ;;("id=\"" . "\""))
                               )
            :content-tags '("<shuowen>" . "</shuowen>")
            :charsets 'lookup-text-cjk-p
            :arranges '((replace support-swjz-arrange-structure))))

;;; support-swjz.el ends here
