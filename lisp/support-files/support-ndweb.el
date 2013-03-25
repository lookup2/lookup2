;;; support-ndweb.el --- ndweb agent support file  -*- lexical-binding: t -*-

;; Copyright (c) KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: 
;; Created: 
;; Keywords:
;; URL: http://lookup2.github.com

;;; Commentary:

;; This file provides appropriate options to each dictionaries.

;;; Code:

(require 'ndweb-options)

(if (string-match "^ndweb:\\(.+?\\)/$" lookup-support-dictionary-id)
    (setq lookup-support-options
          (ndweb-site-options (match-string 1 lookup-support-dictionary-id)
                              lookup-support-agent-options))
  (error "support-ndweb: improper dictionary id=%s" lookup-support-dictionary-id))
  
