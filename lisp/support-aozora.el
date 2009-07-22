;;; support-aozora.el --- support file for "青空文庫" text file.
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

;; This support file provides displaying capability to the "青空文庫"
;; text file.

;;; Code:

(require 'lookup-content)

(defvar support-aozora-use-ivs-font t)

(defun support-aozora-arrange-replace (entry)
  "青空文庫方式に従い、外字を置換し、ルビを設定する。"
  (set-text-properties (point-min) (point-max) nil)
  (goto-char (point-min))
  (while (re-search-forward "※［[^］]+\\([12]\\)-\\([0-9]+\\)-\\([0-9]+\\)］" nil t)
    (let ((plane  (string-to-int (match-string 1)))
          (row    (+ 32 (string-to-int (match-string 2))))
          (column (+ 32 (string-to-int (match-string 3)))))
      (replace-match 
       (char-to-string (make-char (if (= plane 1) 'japanese-jisx0213.2004-1 'japanese-jisx0213-2)
                                  row column)))))
  (goto-char (point-min))
  (while (search-forward "／＼" nil t) (replace-match "〳〵"))
  (goto-char (point-min))
  (while (search-forward "／″＼" nil t) (replace-match "〴〵"))
  (goto-char (point-min))
  (while (re-search-forward "［.+?］" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\(\\(?:｜.+?\\)\\|\\(?:[㐀-鿿󠄀-󠇿々〻※]+\\)\\)《\\(.+?\\)》" nil t)
    (let ((string (match-string 1))
          (ruby (match-string-no-properties 2)))
      (save-match-data (if (= ?｜ (string-to-char string)) (setq string (substring string 1))))
      (put-text-property 0 (length string) 'ruby (cons (length string) ruby) string)
      (put-text-property 0 (1- (length string)) 'read-only t string)
      (replace-match string))))

(defun support-aozora-arrange-fill-lines (entry)
  "テキストをfillし、ルビを表示する。"
  (text-mode)
  (let ((adaptive-fill-regexp nil))
    (while (not (eobp))
     (condition-case nil
         (lookup-arrange-fill-lines entry)
       (error 
        (goto-char (previous-property-change (point)))
        (newline)))
     ))
  (goto-char (point-min))
  ;; Rubyを挿入する。
  (let (start end main main-len 
        ruby-start ruby-end ruby-offset ruby ruby-str
        ruby-start-width ruby-end-width
        (ruby-newline "\n")
        ruby-spc)
    (put-text-property 0 1 'line-height  0.5 ruby-newline)
    (put-text-property 0 1 'line-spacing  0 ruby-newline)
    (while (not (eobp))
      ;; 各行の処理
      (setq start    (point-at-bol)
            ruby-end start
            end      (point-at-eol)
            ruby-str "")
      (while (or ;; 現在位置にルビがある場合
                 (and (get-text-property ruby-end 'ruby)
                      (setq ruby-start ruby-end))
                 ;; 現在位置にルビがない場合
                 (and (setq ruby-start (next-single-char-property-change 
                                        ruby-end 'ruby nil end))
                      (not (equal ruby-start end)))) ;; 行末ではない
        (message "start=%d, ruby-start=%d" start ruby-start)
        (setq ruby       (get-text-property ruby-start 'ruby)
              main-len   (car ruby)
              main       (buffer-substring ruby-start (+ ruby-start main-len))
              ruby       (cdr ruby)
              ruby-spc   (- (* 2.0 (string-width main)) (string-width ruby))
              ruby-end-width   (string-width (buffer-substring start ruby-end))
              ruby-end         (+ ruby-start main-len) ;; new ruby-end
              ruby-start-width (string-width (buffer-substring start ruby-start)))
        (if (< 0 ruby-spc) 
            ;; ルビ文字列が、本文文字列より短い
            (setq ruby-offset 0
                  ruby-spc (/ ruby-spc (length ruby)))
          ;;  ルビ文字列が、本文文字列より長い
          (setq ruby-offset (/ (- ruby-spc) 2)
                ruby-spc 0))
        (setq glue (- (* 2 ruby-start-width) (string-width ruby-str) ruby-offset))
        ;; ruby文字列の作成
        (setq ruby-str 
              (concat ruby-str 
                      (if (< 0 glue) 
                          (concat (make-string (/ (round glue) 2) ?　)
                                  (make-string (% (round glue) 2) ? )))
                      (if (< 0 ruby-spc)
                          (concat (make-string (round (/ ruby-spc 2)) ? )
                                  (mapconcat 'char-to-string 
                                             (string-to-list ruby)
                                             (make-string (round ruby-spc) ? )))
                        ruby))))
      (put-text-property 0 (length ruby-str) 'display '((height 0.5) (width 0.5)) ruby-str)
      (goto-char start)
      (insert ruby-str ruby-newline)
      (forward-line)))
  (let ((inhibit-read-only t))(remove-text-properties (point-min) (point-max) '(read-only)))
  (setq line-spacing 0))

(setq lookup-support-options
      (list 
            :arrange-table '((replace support-aozora-arrange-structure)
                             (fill    support-aozora-arrange-fill))))

;;; support-aozora.el ends here
