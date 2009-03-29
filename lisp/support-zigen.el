;;; support-zigen.el --- suport file for "字源"
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

;; This agent will search the "字源" XML dictionary file for the
;; character.  File can be downloaded from the following site:
;;
;; http://wagang.econ.hc.keio.ac.jp/zigen/
;;
;; Following Program will make index point file, which then can be
;; sorted by 'mksary -s' command.  
;;
;; #!/usr/bin/env ruby -Ku
;; # Usage: ruby zigen.rb all.xml
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if line =~ /^(.*)(<見出字>).+<\/見出字>/ 
;;     print [$offset+$1.length].pack("N")
;;     print [$offset+$1.length+$2.length].pack("N")
;;   end
;;   if line =~ /^(.*)(<見出語>)(.+)<\/見出語>/
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

;;; Usage
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/zigen/all.xml")
;;           ....)

;;; Code:

(require 'lookup)

(defun support-zigen-arrange-structure (entry)
  "Arrange content of ENTRY."
  (goto-char (point-min))
  (while (search-forward "	" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\(</.+?>\\)\n" nil t) (replace-match "\\1"))
  (goto-char (point-min))
  (while (re-search-forward "<見出字>.+</見出字>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 4.0)))))
  (goto-char (point-min))
  (while (re-search-forward "<見出語>.+</見出語>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 2.0)) face lookup-heading-2-face)))
  (goto-char (point-min))
  (while (re-search-forward "<音>.+</音>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(face lookup-heading-3-face)))
  (goto-char (point-min))
  (while (search-forward "<標識>" nil t) (replace-match "《"))
  (goto-char (point-min))
  (while (search-forward "</標識>" nil t) (replace-match "》"))
  (goto-char (point-min))
  (while (search-forward "<音>" nil t) (replace-match "[音]"))
  (goto-char (point-min))
  (while (search-forward "<韻>" nil t) (replace-match "[韻]"))
  (goto-char (point-min))
  (while (search-forward "<字解註>" nil t) (replace-match "[字解註]"))
  (goto-char (point-min))
  (while (re-search-forward "<返点 type=\"\\(.\\)\"/>" nil t)
    (let ((char (match-string 1)))
      (add-text-properties 0 1 '(display ((raise -0.3) (height 0.8))) char)
      (replace-match char)))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n\n+" nil t) (replace-match "\n"))
  (goto-char (point-min))
  (if (looking-at "$") (delete-region (point-min) (1+ (point-min)))))

(setq lookup-support-options
      (list :title "字源"
            :entry-start-end-pairs '(("<見出字>" . "</見出字>")
                                     ("<見出語>" . "</見出語>"))
            :content-start "<漢字>" :content-end "</漢字>"
            :arranges '((replace support-zigen-arrange-structure))))

;;; support-zigen.el ends here
