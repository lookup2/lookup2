;;; support-jtfrk.el --- support file for "字通"「付録」
;; Copyright (C) 2009 KAWABATA Taichi <kawabata.taichi@gmail.com>

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

;;; Memo:

;; 「字通」の付録のうち、以下のファイルは辞書化可能である。
;; b001.html 書名解説
;; b002.html 作者解説
;; f001.html 同訓異字
;; 上記を、(ndjitsuu-convert-html-to-ucs) によってUCSに変換した後、
;; mksary でインデックスを作成する。

;;; Code:

(unless lookup-byte-compiling
  (require 'w3m))

(defun support-jtfrk-arrange-structure (entry)
  (w3m-region (point-min) (point-max)))

(setq lookup-support-options
      (list
       :coding 'utf-8
       :title 
       (cond ((string-match "b001.html" lookup-support-dictionary-id)
              "【字通】書名解説")
             ((string-match "b002.html" lookup-support-dictionary-id)
              "【字通】作者解説")
             ((string-match "f001.html" lookup-support-dictionary-id)
              "【字通】同訓異字"))
       :entry-tags-list '(("【" . "】") ("<small>" . "</small>"))
       :content-tags
       (cond ((string-match "b001.html" lookup-support-dictionary-id)
              '("<A name=\"" . "<!-- honbun end -->"))
             ((string-match "b002.html" lookup-support-dictionary-id)
              '("<A name=\"" . "<BR>"))
             ((string-match "f001.html" lookup-support-dictionary-id)
              '("<A name=\"" . "<br><br>")))
       :code-tags       '("<A name=\"" . "\">")
       :head-tags       '("【" . "】")
       :arranges '((structure support-jtfrk-arrange-structure))))

;;; support-jtfrk.el ends here
