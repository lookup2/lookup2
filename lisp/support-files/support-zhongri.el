;;; support-zhongri.el --- suport file for "中日辞典"  -*- lexical-binding: t -*-

;; Copyright (c) KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: 
;; Created: 
;; Keywords:
;; URL: http://lookup2.github.com

;;; Commentary:

;; This file provides appropriate options to each dictionaries.

;;; Code:

(require 'lookup)
(require 'lookup-text)

(defvar zhongri-hanzi-to-pinyin t)

(defun zhongri-dictionary-arrange-gaiji (entry)
  (let* ((case-fold-search nil)
         (dictionary (lookup-entry-dictionary entry))
         (regexp (lookup-dictionary-gaiji-regexp dictionary))
         start end code)
    (while (re-search-forward regexp nil t)
      (setq start (match-beginning 0) end (match-end 0)
            code (match-string 1))
      (when (string-match "^g\\(..\\)\\(..\\)" code)
          (delete-region start end)
          (goto-char start)
          (insert
           (make-char 'chinese-gb2312 
                      (- (string-to-number (match-string 1 code) 16) 128)
                      (- (string-to-number (match-string 2 code) 16) 128)))))
    (goto-char (point-min))))

(defun zhongri-query-filter (query)
  (lookup-new-query-filter 
   query
   (lambda (string)
     (cons string (lookup-text-hanzi-to-pinyin string)))))

;; If you use "EB Kanji Indexer" 
;; (http://www31.ocn.ne.jp/~h_ishida/EBKIdx.html), you can search 
;; EBXA-C dictionaries with Japanese Kanji.
(setq lookup-support-options
      `(:arranges ((gaiji zhongri-dictionary-arrange-gaiji))
        :query-filter zhongri-query-filter))
        ;:query-filter lookup-query-filter-hanzi-to-pinyin))

;;; support-zhongri.el ends here
