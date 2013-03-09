;;; ndbtonic.el --- Lookup BTONIC dictionary interface -*- lexical-binding: t -*-

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

;;; Documentation:

;; ndbtonic.el provides the suffix array index searching capability
;; for BTONIC dictionary by using `sary' program
;; (http://www.namazu.org/sary/) and BTONIC2xml.rb (v1.10).
;;
;; This agent can work with three kinds of backend agents, 
;; namely, ndsary, ndtext or ndbuffer backends.
;;
;; You can specify backends by `:backend' option.  Default backend is
;; `ndbuffer'.  You must convert BTONIC file to UTF-8 format if you want
;; to use it with `ndtext' backend. 

;;; Usage:

;;  Specify the directory with XXX.xml.ary files.  
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndbtonic "~/edicts/OnMusic" :backend ndbuffer)
;;           ....
;;           ))

;;; Code:

(eval-when-compile (require 'cl))
(require 'ndsary)
(require 'ndbuffer)



;;;
;;; Internal Variables
;;;

(defvar ndbtonic-content-tags '("<dic-item" . "</dic-item>"))
(defvar ndbtonic-code-tags    '(" id=\"" . "\">"))
(defvar ndbtonic-head-tags   '("<headword>" . "</headword>"))
(defvar ndbtonic-entry-tags-list '(("<key>" . "</key>")
                                   (">" . "</headword>")))

;;;
;;; Interface functions
;;;

(put 'ndbtonic :charsets '(ascii japanese-jisx0208))

(put 'ndbtonic :methods 'ndbtonic-dictionary-methods)
(defun ndbtonic-dictionary-methods (_dictionary)
  '(exact prefix suffix substring text))

(put 'ndbtonic :list 'ndbtonic-list)
(defun ndbtonic-list (agent)
  "Return list of dictionaries of btonic AGENT."
  (let* ((location (lookup-agent-location agent))
         (backend  (ndbtonic-agent-backend agent))
         (extension (if (equal backend 'ndsary) ".ary"
                      (or (lookup-agent-option agent :extension)
                          ".xml")))
         (files (directory-files location nil 
                                 (concat (regexp-quote extension) "\\'"))))
    (mapcar (lambda (name) 
              (lookup-new-dictionary agent (file-name-sans-extension name)))
            files)))

(put 'ndbtonic :title 'ndbtonic-title)
(defun ndbtonic-title (dictionary)
  "Get title of DICTIONARY."
  (or (lookup-dictionary-option dictionary :title)
      (let ((name (lookup-dictionary-name dictionary)))
        (file-name-sans-extension name))))

(put 'ndbtonic :search 'ndbtonic-dictionary-search)
(defun ndbtonic-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let* ((string  (lookup-query-string query))
         (method  (lookup-query-method query))
         (dict-id (lookup-dictionary-id dictionary))
         (agent   (lookup-dictionary-agent dictionary))
         (backend (ndbtonic-agent-backend agent))
         (file    (expand-file-name
                   (lookup-dictionary-name dictionary)
                   (lookup-agent-location agent))))
    (loop for (code head val) in 
          (ndtext-search-multiple backend file string method
                                  ndbtonic-content-tags
                                  ndbtonic-entry-tags-list
                                  ndbtonic-head-tags
                                  ndbtonic-code-tags 'cp932-dos)
          for entry = (lookup-new-entry 'regular dictionary code head)
          do (puthash (cons dict-id code) val ndtext-cache)
          collect entry)))

(put 'ndbtonic :content 'ndbtonic-entry-content)
(defun ndbtonic-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((code       (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (file       (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary)))))
    (lookup-with-coding-system 'cp932-dos
      (ndtext-process 'ndsary 'get file code 'exact
                      ndbtonic-content-tags 
                      ndbtonic-code-tags ndbtonic-head-tags
                      ndbtonic-code-tags 'cp932-dos))))

(put 'ndbtonic :arrange-table
     '((replace   ndbtonic-arrange-replace
                  ndbtonic-arrange-image
                  ndbtonic-arrange-audio)
       (gaiji     lookup-arrange-gaijis
                  ndbtonic-arrange-gaiji
                  )
       (reference ndbtonic-arrange-references)
       (structure ndbtonic-arrange-structure
                  lookup-arrange-structure
                  )
       (fill      lookup-arrange-fill-lines
                  ;; ndbtonic-arrange-snd-autoplay
                  )))

;; lookup content-arrangement functions and options
(put 'ndbtonic :gaiji-regexp  "<gi set=\"unicode\" name=\"\\(.+\\)\"/>")
(put 'ndbtonic :gaiji     'ndbtonic-dictionary-gaiji)

;;;
;;; Internal Variables
;;;

(defvar ndbtonic-mojikyo-table
  '((4095 . ?𠹚)
    (65325 . ?𢢫)
    (58234 . ?嚮)))

;;;
;;; Misc functions
;;;

(defun ndbtonic-agent-backend (agent)
  (let ((backend (or (lookup-agent-option agent :backend)
                    'ndbuffer)))
    (unless (memq backend '(ndtext ndsary ndbuffer))
      (error "Improper backend specified! agent=%s" agent))
    backend))

;;;
;;; Formatting Functions
;;;

(defun ndbtonic-arrange-replace (_entry)
  (while (re-search-forward "^[\t ]+" nil t)
    (replace-match "")))

(defun ndbtonic-arrange-image (_entry)
  (while (re-search-forward "<img resid=\"\\(.+?\\)\"/>" nil t)
    (replace-match "【画像】")))

(defun ndbtonic-arrange-audio (_entry)
  (while (re-search-forward "<audio resid=\"\\(.+?\\)\"/>" nil t)
    (replace-match "【音声】")))

(defun ndbtonic-dictionary-gaiji (_dictionary gaiji)
  (list (apply 'string (mapcar (lambda (x) (string-to-number x 16))
                           (split-string gaiji "|" t)))))

(defun ndbtonic-arrange-gaiji (_dictionary)
  ;; for mojikyo characters, etc.
  )

(defun ndbtonic-arrange-references (entry)
  "Arrange reference of ENTRY.
:reference-pattern should be (regexp link-item heading code).
link-item, heading, or code may be integer or function."
  (while (re-search-forward 
          "<ref idref=\"\\(.+?\\)\">\\(.+?\\)</ref>" nil t)
    (let* ((start (match-beginning 0))
           (link (match-string 2))
           (heading (match-string 2))
           (code (concat (car ndbtonic-code-tags)
                         (match-string 1)
                         (cdr ndbtonic-code-tags))))
      (replace-match link t t)
      (setq entry (lookup-new-entry 'regular (lookup-entry-dictionary entry)
                                    code heading))
      (lookup-set-link start (point) entry))))

(defun ndbtonic-arrange-structure (_entry)
  (while (re-search-forward "<key type=\"ソート用かな\">.+?</key>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "<\\(?:span\\|headword\\) type=\"\\(.+?\\)\".*?>\\(.+?\\)</.+?>" nil t)
    (replace-match "\\1:\\2" t))
  (goto-char (point-min))
  (while (re-search-forward "<\\(?:p\\|div\\) type=\"\\(.+?\\)\".*?>" nil t)
    (replace-match "\\1:" t))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (if (looking-at "\n") (delete-region (point-min) (1+ (point-min))))
  )

(provide 'ndbtonic)

;;; ndbtonic.el ends here
