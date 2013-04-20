;;; nddsl.el --- Lookup `DSL' interface -*- lexical-binding: t -*-
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
;; This agent can work with three kinds of backend agents, 
;; namely, ndsary, ndtext or ndbuffer backends.
;;
;; You can specify backends by `:backend' option.  Default backend is
;; `ndbuffer'.  You must convert DSL file to UTF-8 format if you want
;; to use `ndtext' or `ndsary' backend. 
;; 
;; Setup Example:
;;
;; (setq looukp-search-agents
;;       '(
;;         .....
;;         (nddsl "~/edicts/hydcd" :backend ndsary) ; you need "XXX.dsl.ary" for sary backend.
;;         ...))

;;; Code:

(eval-when-compile (require 'cl))
(require 'ndbuffer)
(require 'ndsary)


;;;
;;; Customizable Variables
;;;

(defvar nddsl-minimum-indent 1)

;;;
;;; Internal variables
;;;

(defvar nddsl-entry-tags '("\n" . "\n"))
(defvar nddsl-content-tags '("\n\n" . "\n\n"))
(defvar nddsl-content-start "\n\n")
(defvar nddsl-content-end   "\n\n")
(defvar nddsl-entry-content-end "[")
;;(defun nddsl-content-tags (x)
;;  (if (consp x) '("\n\n" . "[") '("\n\n" . "\n\n")))

;;;
;;; Interface functions
;;;

(put 'nddsl :methods 'nddsl-dictionary-methods)
(defun nddsl-dictionary-methods (_dictionary)
  '(exact prefix suffix substring text))

(put 'nddsl :list 'nddsl-list)
(defun nddsl-list (agent)
  "Return list of dictionaries of AGENT."
  (let* ((location (lookup-agent-location agent))
         (backend  (nddsl-agent-backend agent))
         (extension (if (equal backend 'ndsary) ".ary"
                      (or (lookup-agent-option agent :extension)
                          ".dsl")))
         (files (directory-files location nil
                                 (concat (regexp-quote extension) "\\'"))))
    (message "files=%s" files)
    (if (null files) (error "nddsl: DSL file wity sary index not found! (%s)" location))
    (mapcar (lambda (name)
              (lookup-new-dictionary agent name))
            files)))

(put 'nddsl :title 'nddsl-title)
(defun nddsl-title (dictionary)
  "Return title of DICTIONARY."
  (let* ((name    (lookup-dictionary-name dictionary))
         (agent   (lookup-dictionary-agent dictionary))
         (backend  (nddsl-agent-backend agent))
         (file (expand-file-name name
                (lookup-agent-location 
                 (lookup-dictionary-agent dictionary))))
         (result
          (car (lookup-with-coding-system 'utf-8
                 (ndtext-process backend 'search file "#NAME\t" 'text)))))
    (if (and result 
             (string-match "\"\\(.+\\)\"" (elt result 2)))
        (match-string 1 (elt result 2))
      name)))

(put 'nddsl :search 'nddsl-dictionary-search)
(defun nddsl-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let* ((string  (lookup-query-string query))
         (method  (lookup-query-method query))
         (dict-id (lookup-dictionary-id dictionary))
         (agent   (lookup-dictionary-agent dictionary))
         (backend (nddsl-agent-backend agent))
         (file    (expand-file-name
                   (lookup-dictionary-name dictionary)
                   (lookup-agent-location agent))))
    (loop for (code head val) in 
          (ndtext-process backend 'search file string method
                          nddsl-content-tags nddsl-entry-tags)
          for entry = (lookup-new-entry 'regular dictionary code head)
          do (puthash (cons dict-id code) val ndtext-cache)
          collect entry)))

(put 'nddsl :content 'nddsl-entry-content)
(defun nddsl-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((code       (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (agent      (lookup-dictionary-agent dictionary))
         (backend    (nddsl-agent-backend agent))
         (dict-id    (lookup-dictionary-id dictionary))
         (file (expand-file-name
                (lookup-dictionary-name dictionary)
                (lookup-agent-location agent))))
    (or (gethash (cons dict-id code) ndtext-cache)
        (ndtext-process backend 'get file code 'exact
                        nddsl-content-tags nddsl-entry-tags))))

;;;
;;; Misc functions
;;;

(defun nddsl-agent-backend (agent)
  (let ((backend (or (lookup-agent-option agent :backend)
                    'ndbuffer)))
    (unless (memq backend '(ndtext ndsary ndbuffer))
      (error "Improper backend specified! agent=%s" agent))
    backend))

;;;
;;; Formatting Functions
;;;

(put 'nddsl :arranges
     '((replace   nddsl-arrange-replaces)
       (structure nddsl-arrange-structure)
       (reference nddsl-arrange-reference)
       (fill      nddsl-arrange-fill)))

(defun nddsl-arrange-replaces (_entry)
  (if (looking-at "\n+") (replace-match ""))
  (while (re-search-forward "<<" nil t)
    (replace-match "«") )
  (goto-char (point-min))
  (while (re-search-forward ">>" nil t)
    (replace-match "»")))

(defun nddsl-arrange-structure (_entry)
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
                                    heading
                                    heading)))
      (lookup-put-property entry :dynamic heading)
      (lookup-set-link start end entry))))

(defun nddsl-arrange-fill (_entry)
  (while (re-search-forward "\\[m\\([0-9]\\)\\]" nil t)
    (let ((level (- (string-to-number (match-string 1))
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
