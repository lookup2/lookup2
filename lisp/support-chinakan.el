;;; support-chinakan.el --- support file for 「支那文を讀む爲の漢字典」 data file.
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

;; This support-file will search the 「支那文を讀む爲の漢字典」 data
;; file distributed by Seiwatei Kangojuku (青蛙亭漢語塾), and jumps to
;; the corresponding page image specified by
;; `support-chinakan-url-format'.
;;
;; You may need to make the suffix array index
;; by "mksary" program.  (No option should be attached.)
;;
;; Original site:
;; http://www.seiwatei.net/chinakan/chinakan.cgi
;;
;; Download site:
;; http://www.seiwatei.net/info/dnchina.htm
;;
;; Usage:
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "/path/to/dir_of_`chinadat.csv'")
;;           ....))

;;; Code:

(require 'lookup)

(defvar support-chinakan-url-format
  "http://www.seiwatei.net/chinakan/%s.gif")

(defun support-chinakan-arrange-reference (entry)
  "Arrange contents of ENTRY."
  (goto-char (point-min))
  (insert (lookup-entry-code entry) "\n")
  (while (re-search-forward "^\\([^,]*?\\),\\([^,]*?\\),\\([^,]*?\\),\\([^,]*?\\)," nil t)
    (replace-match
     (concat "\\1"
             (unless (equal (match-string 2) "") "(\\2)")
             (unless (equal (match-string 3) "") " ページ番号：\\3")
             (unless (equal (match-string 4) "") " ページ番号：\\4") ",")))
  (goto-char (point-min))
  (while (re-search-forward "ページ番号：\\([0-9]+\\)" nil t)
    (lookup-url-set-link (match-beginning 0) (match-end 0)
                         (format support-chinakan-url-format
                                 (match-string 1))))
  )

(setq lookup-support-options
      (list :title "China漢"
            :arranges '((reference support-chinakan-arrange-reference))
            :max-hits 100 :regular t))

;;; support-chinakan.el ends here
