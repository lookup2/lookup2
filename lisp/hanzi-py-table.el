;;; hanzi-py-table.el --- Hanzi to Pinyin (simple form) table  -*- lexical-binding: t -*-

;; Copyright (c) KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; URL: http://github.com/kawabata/

;;; Commentary:

;; `hanzi-py-table' provides simple hanzi to pinyin table mainly for
;; dictionary purpose.  This table is extracted from quail files for
;; pinyin input in Emacs leim packages.

;;; Code:

(defvar hanzi-py-table
  (eval-when-compile
    (let ((table (make-char-table 'char-code-property-table)))
      (dolist (file (list (locate-library "quail/TONEPY.el")
                          (locate-library "quail/PY-b5.el")))
        (with-temp-buffer 
          (insert-file-contents file)
          (re-search-forward "(quail-define-rules")
          (goto-char (match-beginning 0))
          (dolist (item (cdar (read-from-string (buffer-substring (point) (point-max)))))
            (let ((py (car item)) (str (cadr item)))
              (dolist (char (string-to-list str))
                (aset table char (push py (aref table char))))))))
      (map-char-table (lambda (k v) (setq v (delete-dups v))
                        (when (= 1 (length v)) (aset table k (car v))))
                      table)
      table)))

(set-char-table-extra-slot hanzi-py-table 0 'hanzi-pinyin)
(define-char-code-property 'hanzi-pinyin hanzi-py-table)

(provide 'hanzi-py-table)
