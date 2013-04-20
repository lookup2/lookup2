;;; ndmecab.el --- Lookup Mecab interface -*- lexical-binding: t -*-
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>
;; Copyright (C) 2009 Lookup Development Team

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>

;; Keywords: dictionary

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Document:

;; This agent use mecab interface of `lookup-text' and mecab itself
;; to display various information on target string.

;;; Code:

(require 'lookup)
(require 'lookup-utils)
(require 'lookup-text)

;;;
;;; Customizable variables
;;;

(defgroup ndmecab nil
  "Lookup MECAB interface."
  :group 'lookup-search-agents)

(defcustom ndmecab-dictionary-title "Mecab 漢字かな辞典"
  "*ndmecab 辞書のタイトル。"
  :type 'string
  :group 'ndmecab)

(defcustom ndmecab-content-format
  `(t "\n" 
    ;; (,lookup-mecab-program "-O" "yomi") "\n" 
    readings "\n" 
    "【解析】" 
    (,lookup-mecab-program "--node-format=[%m|%F-[0,1]]" "--eos-format=\n")
    "\n")
  "*ndmecab 辞書が出力するエントリ内容のフォーマット。
最初にエントリの文字列と「読み」が表示された後の書式を指定できる。
`文字列' - それがそのまま挿入される。
`リスト' - それを引数として MECAB が呼び出され、一行の結果が挿入される。
`t'      - 検索語が挿入される。
'readings - 読みが挿入される。"
  :type '(repeat (radio :tag "出力内容"
			(const :tag "検索語" t)
			(string :tag "文字列")
			(repeat :tag "MECAB 呼び出し"
				(string :tag "option"))))
  :group 'ndmecab)



;;;
;;; Interface functions
;;;

(put 'ndmecab :charsets '(ascii japanese-jisx0208))
(put 'ndmecab :methods '(exact))

(put 'ndmecab :list 'ndmecab-list)
(defun ndmecab-list (agent)
  (if (executable-find lookup-mecab-program)
      (list (lookup-new-dictionary agent lookup-mecab-program))
    (error "ndmecab: error. program file missing.")
    nil))

(put 'ndmecab :title ndmecab-dictionary-title)

(put 'ndmecab :search 'ndmecab-dictionary-search)
(defun ndmecab-dictionary-search (dictionary query)
  (let* ((string (lookup-query-string query))
         (readings (lookup-text-get-kana-readings string)))
    (if readings
        (list (lookup-new-entry 'regular dictionary string)))))

(put 'ndmecab :content 'ndmecab-entry-content)
(defun ndmecab-entry-content (entry)
  (lookup-with-coding-system lookup-mecab-coding-system
    (let ((string (lookup-entry-code entry)))
      (mapconcat (lambda (element)
                   (cond ((eq element t) string)
                         ((eq element 'readings)
                          (mapconcat 
                           'identity (lookup-text-get-kana-readings string) ","))
                         ((stringp element) element)
                         ((listp element) (lookup-get-process-require element string))
                         (t (error "Invalid format element: %S" element))))
                 ndmecab-content-format ""))))


(provide 'ndmecab)

;;; ndmecab.el ends here
