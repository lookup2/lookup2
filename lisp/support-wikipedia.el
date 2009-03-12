;;; support-wikipedia.el --- suport file for "Wikipedia" Summar file.
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

;; This agent will search the wikipedia summary file.  You will need
;; to make the sufary-index by "mksary" program.
;;
;; Unfortunately, current English wikipedia summary file exceeds
;; 2Gbyte, so you must split it in half (by `split' command, for
;; example), to make it less than 2Gbyte to use it.  (sary can not
;; handle more than 2G file.)
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
;; # Usage: ruby wikipedia.rb < wiki.xml > wiki.xml.ary
;; file = $stdin
;; $offset=0
;; file.each_line{|line|
;;   if line =~ /^(<title>Wikipedia: )(.+)<\/title>/ 
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
(require 'lookup-content) ; for `lookup-content-mode-map'

(defvar support-wikipedia-link-map nil)

(defun support-wikipedia-arrange-structure (entry)
  "Attach contents of ENTRY a link and remove tags."
  ;; initialization
  (unless support-wikipedia-link-map
    (setq support-wikipedia-link-map (copy-keymap lookup-content-mode-map))
    (define-key support-wikipedia-link-map "\C-m" 'support-wikipedia-follow-link))
  ;; initialization end
  (goto-char (point-min))
  (while (re-search-forward "<url>\\(.+?\\)</url>" nil t)
    (let ((link (match-string 1))
          (start (match-beginning 0)))
      (replace-match "《→リンク》")
      (support-wikipedia-set-link start (point) link)))
  (goto-char (point-min))
  (while (re-search-forward
          (concat "<sublink.+?>.*?"
                  "<anchor>\\(.+?\\)</anchor>.*?"
                  "<link>\\(.+?\\)</link></sublink>") nil t)
    (let ((start (match-beginning 0))
          (anchor (match-string 1))
          (link (match-string 2)))
      (replace-match anchor)
      (support-wikipedia-set-link start (point) link)))
  (goto-char (point-min))
  (while (re-search-forward "</?doc>[\t\n]+" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<title>Wikipedia: " nil t) (replace-match ""))
  (goto-char (point-min))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t) (replace-match "")))

(defun support-wikipedia-follow-link ()
  (interactive)
  (let ((url (get-text-property (point) 'support-wikipedia-link)))
    (browse-url url)))

(defun support-wikipedia-set-link (start end uri)
  (add-text-properties start end
                       (list 'keymap support-wikipedia-link-map
                             'face 'lookup-reference-face
                             'mouse-face 'highlight
                             'help-echo uri
                             'lookup-tab-stop t
                             'support-wikipedia-link uri)))

(setq lookup-support-options
      (list :title (cond ((string-match "/jawiki" lookup-support-dictionary-id)
                          "Wikipedia 日本語")
                         ((string-match "/enwiki" lookup-support-dictionary-id)
                          "Wikipedia English")
                         ((string-match "/frwiki" lookup-support-dictionary-id)
                          "Wikipedia Française")
                         ((string-match "/zhwiki" lookup-support-dictionary-id)
                          "Wikipedia 中文")
                         (t "Wikipedia"))
            :entry-start "<title>Wikipedia: " 
            :entry-end "</title>"
            :content-start "<doc>" :content-end "</doc>"
            :arranges '((reference support-wikipedia-arrange-structure))
            :max-hits 100))

;;; support-wikipedia.el ends here
