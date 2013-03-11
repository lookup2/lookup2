;;; support-pdic-thai.el --- Support for PDIC Thai -*- lexical-binding: t -*-

;; Copyright (c) KAWABATA, Taichi

;;; Commentary:

;; support for PDIC Thai
;; http://www.pdicthai.com/~pdicthai/

;;; Code:

(require 'cl-lib)
(require 'lookup)

(defvar support-pdic-thai-title-alist
  '(("Uni-PdicThai-IPA" :title "タイ日辞書")
    ("Uni-PdicThai-JP-IPA" :title "タイ日辞書（かな）" 
     :query-filter lookup-query-filter-kanji-to-kana)
    ("Uni-PdicThai-RS-IPA" :title "タイ日辞書（ローマ字）")))

(setq lookup-support-options
      (cdr
       (cl-assoc-if 
        (lambda (x) (string-match x lookup-support-dictionary-id))
        support-pdic-thai-title-alist)))
