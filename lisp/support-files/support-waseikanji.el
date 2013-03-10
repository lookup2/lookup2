;;; support-waseikanji.el --- suport file for "和製漢字の辞典" -*- coding: utf-8 -*-
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

;; This file support searching the "和製漢字の辞典" HTML dictionary
;; file.  The dictionary can be downloaded by the following shell
;; script.
;;
;;
;; #/bin/sh
;; i=1
;; while [ $i -le 24 ]; do
;; wget http://homepage2.nifty.com/TAB01645/ohara/p$(printf '%02d' $i).htm
;; i=`expr $i + 1`
;; done
;; cat p*.htm > waseikanji.html
;; rm p*.htm
;; 
;;
;; After generating the "waseikanji.html", set the variable
;; 'support-waseikanji-ids-file' (file can be obtained from
;; http://kanji-database.cvs.sourceforge.net/viewvc/kanji-database/kanji-database/data/waseikanji-ids.txt)
;; and execute 'M-x support-waseikanji-convert-index' on
;; "waseikanji.html", and save the file in UTF-8 encoding.  After
;; then, execute `mksary' program on "waseikanji.html" file to
;; generage "waseikanji.html.ary" suffix array file.

;;; Usage
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/waseikanji")
;;           ....))

;;; Code:

(defvar support-waseikanji-ids-file
  (expand-file-name "~/edicts/WaseiKanji/waseikanji-ids.txt"))

(defun support-waseikanji-convert-index ()
  (interactive)
  (let ((code-to-chars (make-hash-table :test 'equal))
        (chars-to-code (make-hash-table :test 'equal))
        (code-to-ids   (make-hash-table :test 'equal))
        ids code)
    (with-temp-buffer
      (insert-file-contents support-waseikanji-ids-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+?\\)	\\(.+?\\)\\(	.*\\)?$" nil t)
        (puthash (match-string 1) (match-string 2) code-to-ids)))
    (goto-char (point-min))
    (while (re-search-forward "ISO-2022-JP" nil t)
      (replace-match "UTF-8"))
    (goto-char (point-min))
    (while 
        (re-search-forward "src=\"k\\(.+?\\)\\.gif.+?alt=\"【\\(.+?\\)】" nil t)
      (puthash (match-string 1) (match-string 2) code-to-chars)
      (puthash (match-string 2) (match-string 1) chars-to-code)
      (setq ids (gethash (match-string 1) code-to-ids))
      (if ids (replace-match ids nil nil nil 2)))
    (goto-char (point-min))
    (while (re-search-forward "［\\(.+?\\)］" nil t)
      (setq code (gethash (match-string 1) chars-to-code))
      (if code (replace-match (gethash code code-to-ids) nil nil nil 1)))))

(defun support-waseikanji-arrange-structure (entry)
  "Attach contents of ENTRY a link and remove tags."
  (while (re-search-forward "<img.+?\\(【.+?】\\).+?>" nil t)
    (replace-match (match-string 1)))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t)
    (replace-match " "))
  (goto-char (point-min))
  (if (looking-at " *\n *") (replace-match "")))

(setq lookup-support-options
      (list :title "和製漢字の辞典"
            :query-filter 'lookup-query-filter-to-katakana
            :entry-tags-list '(("【" . "】")
                               ("「" . "」")
                               )
            :content-tags '("<a name=" . "</TABLE>")
            :code-tags '("<a name=\"" . "\"></a>")
            :head-tags '("【" . "】")
            :coding 'utf-8
            ;; :charsets 'lookup-text-cjk-p
            :arranges '((replace support-waseikanji-arrange-structure))))

;;; support-waseikanji.el ends here
