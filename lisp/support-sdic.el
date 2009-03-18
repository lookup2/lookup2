;;; support-sdic.el --- suport file for `sdic' format file
;; Copyright (C) 2009 Lookup Development Team

;; Author: KAWABATA, Taichi

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

;; This agent will search sdic dictionary files.  Many linux
;; distributions provide packages for the sdic dictionaries.
;;
;; Following Program will make index point file, which then can be
;; sorted by 'mksary -s' command.  
;;
;; #!/usr/bin/env ruby -Ke
;; # Usage: ruby sdic.rb gene.sdic
;; STDIN.reopen(ARGV[0], "r")
;; STDOUT.reopen(ARGV[0]+".ary", "w")
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if line =~ /^(.*)(<K>)(.+)<\/K>/ 
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
;;           (ndsary "/path/to/gene.sdic") ; make sure it is sary-ed.
;;           ....)
;;
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsimple "/path/to/gene.sdic")
;;           ....)

;;; Code:

(require 'lookup)

(defun support-sdic-arrange-structure (entry)
  "Arrange content of ENTRY."
  (goto-char (point-min))
  (while (re-search-forward "^.*<H>\\(.+\\)</H>.*<K>.+</K>\\(.*\\)" nil t)
    (replace-match "\\1\n\\2\n" t))
  (while (re-search-forward "^.*<K>\\(.+\\)</K>\\(.*\\)" nil t)
    (replace-match "\\1\n\\2\n" t)))

(setq lookup-support-options
      (list :entry-start "<K>" :entry-end "</K>"
            :title "SDIC"
            :arranges '((replace support-sdic-arrange-structure))
            :coding 'euc-jp))

;;; support-sdic.el ends here
