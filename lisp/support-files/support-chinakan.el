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
;;  - If you use `mksary'
;;
;;   (setq lookup-search-agents
;;         '(....
;;           (ndsary "/path/to/dir_of_`chinadat.csv'")
;;           ....))
;;
;;  - If you do not use `mksary'
;;
;;   (setq lookup-search-agents
;;         '(....
;;           (ndtext "/path/to/dir_of_`chinadat.csv'"
;;            :extension ".csv"
;;           ....))

;;; Code:

(defvar support-chinakan-url-format
  "http://www.seiwatei.net/chinakan/%s.gif")

(defvar support-chinakan-field-format
  '("文字"
    "参照文字"
    "支那漢のページ"
    "参照文字のページ"
    "部首コード"
    "部首内画数"
    "総画数"
    "四角号碼"
    "ピンイン"
    "日本語音訓"))

;;;
;;; Main Program
;;;

(defun support-chinakan-entry-tags-list (string method)
  (if (string-match "^[ァ-ヺあ-ん]+$" string) '(("1" . "1")) '(("" . ""))))

(defun support-chinakan-arrange-reference (entry)
  "Arrange contents of ENTRY."
  (while (search-forward "\n\n" nil t) (replace-match ""))
  (goto-char (point-min))
  (insert (lookup-entry-code entry) "\n")
  (while (re-search-forward "^.+?,.+?,.+$" nil t)
    (let ((data (match-string 0))
          (field support-chinakan-field-format))
      (replace-match "")
      (setq data (split-string data ","))
      (while (and data field)
        (if (and (car data) (not (equal (car data) "")))
            (insert (car field) ": " (car data) "\n"))
        (setq data (cdr data)
              field (cdr field)))))
  (goto-char (point-min))
  (while (re-search-forward "ページ: \\([0-9]+\\)" nil t)
    (lookup-set-link (match-beginning 0) (match-end 0)
                     (lookup-new-entry
                      'url (lookup-entry-dictionary entry)
                      (format support-chinakan-url-format
                              (match-string 1))))))

(setq lookup-support-options
      (list :title "支那文を讀む爲の漢字典"
            :coding 'utf-8-dos
            :charsets '(han kana)
            :entry-tags-list 'support-chinakan-entry-tags-list
            :content-tags '("\n" . "\n")
            :code-tags  '("" . ",")
            :arranges '((reference support-chinakan-arrange-reference))))

;;; support-chinakan.el ends here
