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

(when (equal lookup-support-agent "ndweb")
  (setq lookup-support-options
        (ndweb-site-options (match-string 1 lookup-support-dictionary-id)
                            lookup-support-agent-options)))
