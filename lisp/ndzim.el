;;; ndzim.el --- Lookup ZIM file front-end  -*- lexical-binding: t -*-
;; Copyright (c) KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Lookup OpenZIM agent
;; Created: 2013/03/07
;; Keywords: OpenZIM, Wikipedia
;; Version: 0.0.1
;; Package-version: 0.0.1
;; URL: http://lookup2.github.com/

;;; Commentary:

;; This agent provides searching capabilities for OpenZIM file.
;; for details of OpenZIM format, see http://www.openzim.org/

;; Specify the location of XXX.zim in lookup-search-agents variable.
;; e.g.
;; (setq lookup-search-agents
;;       '(...
;;         (ndzim "/path/to/dir")
;;         ...))

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'cl-lib))
(require 'lookup)
(require 'ndweb) ;; w3m related functions and variables

(defconst ndzim-version "0.1")

(defvar ndzim-search         "zimsearch")
(defvar ndzim-search-options '("--weight-dist" "0"))
(defvar ndzim-dump           "zimdump")

(put 'ndzim :methods '(exact prefix))

;;;
;:: Interface functions
;;;
(put 'ndzim :list 'ndzim-list)
(defun ndzim-list (agent)
  (ndzim--check-environment)
  (let* ((dir (lookup-agent-location agent)))
    (when (file-directory-p dir)
      (loop for file in (directory-files dir nil "\\.zim$")
            collect (lookup-new-dictionary agent file)))))

(put 'ndzim :title 'ndzim-title)
(defun ndzim-title (dictionary)
  (or (lookup-dictionary-option dictionary :title)
      (file-name-sans-extension
       (lookup-dictionary-name dictionary))))

(put 'ndzim :search 'ndzim-dictionary-search)
(defun ndzim-dictionary-search (dictionary query)
  (ndzim--check-environment)
  (let* ((string  (lookup-query-string query))
         (method  (lookup-query-method query))
         (file    (expand-file-name
                   (lookup-dictionary-name dictionary)
                   (lookup-agent-location
                    (lookup-dictionary-agent dictionary)))))
    (loop for (code head) in (ndzim-search string method file)
          collect (lookup-new-entry 'regular dictionary code head))))

(put 'ndzim :content 'ndzim-dictionary-content)
(defun ndzim-dictionary-content (entry)
  (ndzim--check-environment)
  (let* ((dictionary (lookup-entry-dictionary entry))
         (file       (ndzim--dictionary-file dictionary))
         (code       (lookup-entry-code entry))
         (info       (ndzim-info nil code file)))
    (multiple-value-bind (_url _title redirection) info
      (when redirection
        (setq code (car redirection))))
    (ndzim-content code file)))

(put 'ndzim :arrange-table '((reference ndzim-arrange-references
                                        ndzim-arrange-image
                                        ndweb-arrange-tags)
                             (fill      lookup-arrange-nofill)))

(put 'ndzim :clear 'ndzim-clear)
(defun ndzim-clear (_dictionary)
  (let ((directory (concat temporary-file-directory "/ndzim/")))
    (when (file-directory-p directory)
      (delete-directory directory t t))))

;;; Supplementary Functions

(defun ndzim--dictionary-file (dictionary)
  (let ((agent (lookup-dictionary-agent dictionary)))
    (expand-file-name
     (lookup-dictionary-name dictionary)
     (lookup-agent-location agent))))

(defun ndzim--check-environment ()
  (dolist (exec (list ndzim-search ndzim-dump ndweb-w3m))
    (unless (executable-find exec)
      (error "ndzim: required application `%s' can not be found." exec))))

;;;
;;; Internal Functions
;;; 

(defun ndzim-dump-url (url file)
  (unless (string-match "^[-ABIJMUVWX]/" url) (error "Improper ZIM URL!"))
  (let* ((url-file (concat temporary-file-directory "/ndzim/" url))
         (directory (file-name-directory url-file)))
    (unless (file-directory-p directory) (make-directory directory t))
    (lookup-debug-message "url-file=%s" url-file)
    (unless (file-exists-p url-file)
      (call-process ndzim-dump nil (list :file url-file) nil
                    "-u" url "-d" (file-truename file)))
    url-file))

(defun ndzim-search (string method file)
  (with-temp-buffer
    (call-process ndzim-search nil (current-buffer) nil 
                  (file-truename file) string)
    (let (result (count 0) (max (if (equal method 'exact) 1 lookup-max-hits)))
      (goto-char (point-min))
      (while (and (re-search-forward "^article \\([0-9]+\\).+?:\t\\(.+\\)" nil t)
                  (< count max))
        ;; 正式には ndzim-info を使って URL を正式に取得するのが筋だが高速化のため
        ;; URLは "A/<title>.html" だと仮定する。
        (push (list (concat "A/" (match-string 2) ".html") (match-string 2)) result)
        (incf count))
      (nreverse result))))

(defun ndzim-info (index url file)
  "Get URL, Title, Redirection (URL, Title) if exists."
  (with-temp-buffer
    (if index
        (call-process ndzim-dump nil (current-buffer) nil
                      "-o" index "-i" (file-truename file))
        (call-process ndzim-dump nil (current-buffer) nil
                      "-u" url "-i" (file-truename file)))
    (goto-char (point-min))
    (let (url title namespace redirection)
      (re-search-forward "url: *\\(.+\\)")
      (setq url (match-string 1))
      (re-search-forward "title: *\\(.+\\)")
      (setq title (match-string 1))
      (re-search-forward "namespace: *\\(.*\\)")
      (setq namespace (match-string 1))
      (if (re-search-forward "redirect index: *\\(.*\\)" nil t)
          (setq redirection (match-string 1)))
      (if redirection
          (cl-callf ndzim-info redirection nil file))
      (list (concat namespace "/" url) title redirection))))

(defun ndzim-content (url file)
  (let* ((url-file (ndzim-dump-url url file))
         (args (append ndweb-w3m-options (list url-file))))
    (with-temp-buffer
      ;;(lookup-debug-message "w3m args=%s" args)
      (apply 'call-process ndweb-w3m nil (current-buffer) nil args)
      (buffer-string))))

;;;
;;; Arrange Functions
;;;

(defun ndzim-arrange-references (entry)
  (let ((dictionary (lookup-entry-dictionary entry))
        (case-fold-search t))
    (while (re-search-forward "<a .*?href=\"\\(.+?\\)\".*?>\\(.+?\\)</a>" nil t)
      (let* ((href (match-string 1))
             (ref (save-match-data
                    (if (string-match "^/A/\\(.+\\)\\.html$" href)
                        (match-string 1 href)))))
        (lookup-set-link (match-beginning 2) (match-end 2)
                         (if ref (lookup-new-entry 'regular dictionary
                                                   (concat "A/" ref ".html") ref)
                           (lookup-new-entry 'url dictionary href href)))
        (delete-region (match-end 2) (match-end 0))
        (delete-region (match-beginning 0) (match-beginning 2))))
    (goto-char (point-min))))

(defun ndzim-arrange-image (entry)
  (let* ((dictionary (lookup-entry-dictionary entry))
         (file (ndzim--dictionary-file dictionary))
         (case-fold-search t))
    (while (re-search-forward
            "<img_alt .*?src=\"/\\(.+?\\)\".*?>\\(.+?\\)</img_alt>" nil t)
      (let* ((img-url (match-string 1))
             (img-file (save-match-data (ndzim-dump-url img-url file)))
             (img-type (save-match-data
                         (if (string-match "\\.jpg" img-file) 'jpeg 'png))))
        (lookup-img-file-insert img-file img-type
                                (match-beginning 0) (match-end 0))))))

(provide 'ndzim)
