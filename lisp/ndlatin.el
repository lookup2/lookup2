;;; ndlatin.el --- agents for `word-latin' -*- lexical-binding: t -*-
;; Copyright (C) 2007 Lookup Development Team <lookup@ring.gr.jp>

;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Version: $Id: ndlatin.el,v 1.5 2009/05/12 00:59:47 kawabata Exp $

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

;; This is a lookup interface for "WORDS (LATIN)", written by 
;; William Whitaker (whitaker@erols.com).  It works as both
;; Latin-English and English-Latin dictionary.  You may obtain 
;; the program from <http://users.erols.com/whitaker/wordsdoc.htm>.
;; It works with all Macintosh/Windows/Linux, but you may need to
;; check the custom variables to work it properly.

;;; Usage:

;; Before using this agent, please change the
;; "PAUSE_IN_SCREEN_OUTPUT_HELP" parameter of `words' to `No' by
;; running the program, type '!' and  ENTER, change the specified 
;; parameter, and then save the changes.
;;
;; Simply put the `ndlatin' entry in your ~/.lookup/init.el file.
;; Location must be specified, of your installed `words' program.
;;
;;   ;; for Linux/Windows
;;   (setq ndlatin-word-program "~/bin/words/words.exe")
;;   ;; for Macintosh
;;   (setq ndlatin-word-program "/Applications/Interpres.app/Contents/Resources/words")
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndlatin)
;;           ....
;;           ))

;;; Code:

(require 'cl-lib)
(require 'lookup)
(defconst ndlatin-version "0.2")


;;;
;;; customizable variables
;;;

(defgroup ndlatin nil
  "Lookup ndlatin interface."
  :group 'lookup-agents)

(defcustom ndlatin-program
  "/Applications/Interpres.app/Contents/Resources/words"
  "Location of Latin WORDS program"
  :type 'string
  :group 'ndlatin)
;; /Library/Application Support/words/words

(defcustom ndlatin-splitter 
  (if (eq system-type 'windows-nt) ";$" "")
  "Splitting regexp for output results.
For Macintosh/Linux version, '' would be better, while for
Windows version, ';$' may be appropriate (but it sometimes
fails)."
  :type 'string
  :group 'ndlatin)

(defcustom ndlatin-dic-change-char "-"
  "Dictionary Change Command character.
For Macintosh version (included in Interpres), it should be '-', while for
Windows/Linux version (ver 1.97), it should be '~'"
  :type 'string
  :group 'ndlatin)


;;;
;:: Interface
;;;

(put 'ndlatin :list     'ndlatin-list)
(put 'ndlatin :kill     'ndlatin-kill)
(put 'ndlatin :title    'ndlatin-dictionary-title)
(put 'ndlatin :methods  '(exact))
(put 'ndlatin :charsets '(ascii))
(put 'ndlatin :search   'ndlatin-dictionary-search)
(put 'ndlatin :content  'ndlatin-entry-content)

;;;
;;; Internal variables
;;;

(defvar ndlatin-prompt "=>")
(defvar ndlatin-process nil)
(defconst ndlatin-dictionary-table
  '(("el" "Words/English-Latin Dictionary")
    ("le" "Words/Latin-English Dictionary")))

;;;
;;; interface functions
;;;

(defun ndlatin-list (agent)
  (ndlatin-process-open)
  (mapcar (lambda (e) (lookup-new-dictionary agent (car e)))
          ndlatin-dictionary-table))

(defun ndlatin-kill (_agent)
  (if ndlatin-process
      (lookup-process-kill ndlatin-process)))

(defun ndlatin-dictionary-title (dictionary)
  (ndlatin-process-open)
  (cadr (assoc (lookup-dictionary-name dictionary)
               ndlatin-dictionary-table)))

(defun ndlatin-dictionary-search (dictionary query)
  "Search Lat-Eng or Eng-Lat DICTIONARY for QUERY."
  (ndlatin-process-open)
  (let ((dictname (lookup-dictionary-name dictionary))
        (string   (lookup-query-string query))
        (output))
    (if (equal dictname "le")
        (lookup-process-require
         ndlatin-process (concat ndlatin-dic-change-char "L\n")
         ndlatin-prompt)
      (lookup-process-require
       ndlatin-process (concat ndlatin-dic-change-char "E\n")
       ndlatin-prompt))
    (setq output
          (lookup-process-require
           ndlatin-process (concat string "\n") ndlatin-prompt))
    (when (not (or (string-match "No Match" output)
                   (string-match "UNKNOWN" output)))
      (setq output (replace-regexp-in-string "\n\n+" "\n" output))
      (setq output (split-string output ndlatin-splitter t))
      (setq output (delete-dups output))
      (setq output
            (cl-remove-if ;; remove empty output
             (lambda (x) (not (string-match "[a-zA-Z]" x))) output))
      (mapcar (lambda (x) (lookup-new-entry 'regular dictionary
                                            x string))
              output))))

(defun ndlatin-entry-content (entry)
  "Return ndlatin ENTRY content."
  (concat (lookup-entry-heading entry)
          "\n" (lookup-entry-code entry)))

;;;
;;; utility functions
;;;

(defun ndlatin-process-open ()
  "Open Latin Words process."
  (unless (file-executable-p ndlatin-program)
    (error "`ndlatin-program' (%s) is not properly specified!" ndlatin-program))
  (unless (and (processp ndlatin-process)
               (eq (process-status ndlatin-process) 'run))
    (let ((buffer (or (lookup-open-process-buffer " *ndlatin*")
                      (lookup-temp-buffer))))
      (with-current-buffer buffer
        ;; specify default working directory by `default-directory'
        (let ((default-directory
                (directory-file-name ndlatin-program)))
          (setq ndlatin-process
                (start-process "ndlatin" buffer
                               ndlatin-program))
          (set-process-coding-system ndlatin-process 'utf-8 'utf-8)
          (set-process-query-on-exit-flag ndlatin-process nil)
          (catch 'started
            (while (accept-process-output ndlatin-process 10)
              (save-excursion
                (goto-char (point-min))
                (when (search-forward ndlatin-prompt nil t)
                  (throw 'started t))))
            (error "Failed start process"))))
      (unless lookup-enable-debug
	(set-process-buffer ndlatin-process nil)
	(kill-buffer buffer)))
    (lookup-process-require ndlatin-process "\n" ndlatin-prompt)
    ))

(provide 'ndlatin)

;;; ndlatin.el ends here
