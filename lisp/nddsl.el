;;; nddsl.el --- Lookup `dsl' interface -*- coding: utf-8 -*-
;; Copyright (C) 2009 KAWABATA Taichi <kawabata.taichi@gmail.com>

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

;; nddsl.el provides the suffix array index searching capability
;; for DSL (dictionary specification language) format dictionaries.
;; DSL is specified by Lingvo (http://www.lingvo.com/).
;;
;; To use this agent, you must convert DSL file (UTF-16LE) to UTF-8
;; encoding, then make suffix array for them.
;; 
;; Example:
;; % nkf -Lu -w -W16 < dict.dsl > dict.utf8.dsl
;; % mksary -c utf-8 dict.utf8.dsl
;;
;; Setup Example:
;;
;; (setq looukp-search-agents
;;       '(
;;         .....
;;         (nddsl "~/edicts/hydcd")  ; directory where `*.dsl.ary' file exists.
;;         ...))

;;; Code:

(require 'lookup)
(require 'ndsary)


;;;
;;; Customizable Variables
;;;

(defvar nddsl-sary-program "sary")
(defvar nddsl-sary-program-options '())
(defvar nddsl-minimum-indent 1)

;;;
;;; Internal variables
;;;

(defvar nddsl-entry-tags '("\n" . "\n"))
(defvar nddsl-content-start "\n\n")
(defvar nddsl-content-end   "\n\n")
(defvar nddsl-entry-content-end "[")
(defun nddsl-content-tags (x)
  (if (consp x) '("\n\n" . "[") '("\n\n" "\n\n")))

;;;
;;; Interface functions
;;;

(put 'nddsl :methods 'nddsl-dictionary-methods)
(defun nddsl-dictionary-methods (dictionary)
  '(exact prefix suffix substring text))

(put 'nddsl :list 'nddsl-list)
(defun nddsl-list (agent)
  "Return list of dictionaries of AGENT."
  (let* ((files (directory-files 
                 (expand-file-name (lookup-agent-location agent))
                 nil "\\.dsl\\.ary\\'")))
    (mapcar (lambda (name) 
              (lookup-new-dictionary agent (file-name-sans-extension name)))
            files)))

(put 'nddsl :title 'nddsl-title)
(defun nddsl-title (dictionary)
  "Return title of DICTIONARY."
  (let* ((name (lookup-dictionary-name dictionary))
         (file (expand-file-name name
                (lookup-agent-location 
                 (lookup-dictionary-agent dictionary))))
         (result
          (lookup-with-coding-system 'utf-8
            (ndsary-file-content file "#NAME"))))
    (if (and result 
             (string-match "\"\\(.+\\)\"" result))
        (match-string 1 result)
      name)))

(put 'nddsl :search 'nddsl-dictionary-search)
(defun nddsl-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let ((string      (lookup-query-string query))
        (method      (lookup-query-method query))
        (file        (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
        entries)
    (setq entries
          (lookup-with-coding-system 'utf-8
            (ndsary-file-search
             file string method nddsl-entry-tags
             'nddsl-content-tags nddsl-entry-tags)))
    (mapcar
     (lambda (x) (lookup-new-entry
                  'regular dictionary (car x) (cdr x)))
     entries)))

(put 'nddsl :content 'nddsl-entry-content)
(defun nddsl-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((code (lookup-entry-code entry))
         (dict (lookup-entry-dictionary entry))
         (file (expand-file-name
                (lookup-dictionary-name dictionary)
                (lookup-agent-location
                 (lookup-dictionary-agent dictionary)))))
    (lookup-with-coding-system 'utf-8
      (ndsary-file-content
       file (concat (car nddsl-entry-tags) code (cdr nddsl-entry-tags))
       nddsl-content-start nddsl-content-end))))

;;;
;;; Formatting Functions
;;;

(put 'nddsl :arranges
     '((replace   nddsl-arrange-replaces)
       (structure nddsl-arrange-structure)
       (reference nddsl-arrange-reference)
       (fill      nddsl-arrange-fill)))

(defun nddsl-arrange-replaces (entry)
  (if (looking-at "\n+") (replace-match ""))
  (while (re-search-forward "<<" nil t)
    (replace-match "«") )
  (goto-char (point-min))
  (while (re-search-forward ">>" nil t)
    (replace-match "»")))

(defun nddsl-arrange-structure (entry)
  (if (re-search-forward "^[ \t]" nil t)
      (lookup-make-region-heading (point-min) (1- (match-beginning 0)) 1))
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]+" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\[b.*?\\]" nil t)
    (let ((start (match-beginning 0))
          (end   (or (and (re-search-forward "\\[/b\\]" nil t)
                          (match-end 0))
                     (point-max))))
      (lookup-make-region-heading start end 2)))
  (goto-char (point-min))
  (while (re-search-forward "\\[u.*?\\]" nil t)
    (let ((start (match-beginning 0))
          (end   (or (and (re-search-forward "\\[/u\\]" nil t)
                          (match-end 0))
                     (point-max))))
      (lookup-make-region-heading start end 3)))
  (goto-char (point-min))
  (while (re-search-forward "\\[c.*?\\]" nil t)
    (let ((start (match-beginning 0))
          (end   (or (and (re-search-forward "\\[/c\\]" nil t)
                          (match-end 0))
                     (point-max))))
      (lookup-make-region-heading start end 4)))
  (goto-char (point-min))
  (while (re-search-forward "\\[ex*?\\]" nil t)
    (let ((start (match-beginning 0))
          (end   (or (and (re-search-forward "\\[/ex\\]" nil t)
                          (match-end 0))
                     (point-max))))
      (lookup-make-region-heading start end 6))))

(defun nddsl-arrange-reference (entry)
  (while (re-search-forward "\\[ref\\]" nil t)
    (let* ((dict (lookup-entry-dictionary entry))
           (start (match-end 0))
           (end   (or (and (re-search-forward "\\[/ref\\]" nil t)
                           (match-beginning 0))
                      (point-max)))
           (heading (buffer-substring-no-properties start end))
           (entry (lookup-new-entry 'regular dict
                                    (concat "\n" heading "\n")
                                    heading)))
      (lookup-put-property entry :dynamic code)
      (lookup-set-link start end entry))))

(defun nddsl-arrange-fill (entry)
  (while (re-search-forward "\\[m\\([0-9]\\)\\]" nil t)
    (let ((beg-beg (match-beginning 0))
	  (beg-end (match-end 0))
	  (level (- (string-to-number (match-string 1))
		     nddsl-minimum-indent))
	  indent-end point)
      (when (> level 0)
	(setq point (point))
	(setq indent-end
	      (or (and (re-search-forward "\\[\m\\]" nil t)
		       (match-end 0))
		  (point-max)))
	(set-left-margin point indent-end level)
	(goto-char point))))
  ;; remove all tags
  (goto-char (point-min))
  (while (re-search-forward "\\[.+?\\]" nil t)
    (replace-match "")))

(provide 'nddsl)

;;; nddsl.el ends here
