;;; ndlatin.el --- agents for `word-latin'
;; Copyright (C) 2007 Lookup Development Team <lookup@ring.gr.jp>

;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>
;; Version: $Id: ndlatin.el,v 1.2 2009/03/07 17:34:07 kawabata Exp $

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
;;  Example: (for Macintosh)
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndlatin "/Library/Application Support/words/words")
;;           ....
;;           ))
;;
;;  Example: (for Windows & NTEmacs)
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndlatin "~/bin/words/words.exe")
;;           ....
;;           ))


;;; Code:

(require 'lookup)
(defconst ndlatin-version "0.1")


;;;
;;; customizable variables
;;;

(defgroup ndlatin nil
  "Lookup ndlatin interface."
  :group 'lookup-agents)

(defcustom ndlatin-splitter 
  (if (eq system-type 'windows-nt) ";$" "")
  "Splitting regexp for output results.
For Macintosh/Linux version, '' would be better, while for
Windows version, ';$' may be appropriate (but it sometimes
fails)."
  :type 'string
  :group 'ndlatin)

(defcustom ndlatin-dic-change-char "~"
  "Dictionary Change Command character.
For Macintosh version (ver 1.93), it should be '>', while for
Windows/Linux version (ver 1.97), it should be '~'"
  :type 'string
  :group 'ndlatin)

;;;
;:: Interface
;;;

(put 'ndlatin :list     #'ndlatin-list)
(put 'ndlatin :kill     #'ndlatin-kill)
(put 'ndlatin :title    #'ndlatin-dictionary-title)
(put 'ndlatin :methods  '(exact))
(put 'ndlatin :search   #'ndlatin-dictionary-search)
(put 'ndlatin :content  #'ndlatin-entry-content)

;;;
;;; Internal variables
;;;

(defvar ndlatin-program nil)
(defvar ndlatin-prompt "=>")
(defvar ndlatin-process nil)
(defconst ndlatin-dictionary-table
  '(("el" "Words/English-Latin Dictionary")
    ("le" "Words/Latin-English Dictionary")))

;;;
;;; interface functions
;;;

(defun ndlatin-list (agent)
  (ndlatin-process-open agent)
  (mapcar (lambda (e) (lookup-new-dictionary agent (car e)))
          ndlatin-dictionary-table))

(defun ndlatin-kill (agent)
  (if ndlatin-process
      (lookup-process-kill ndlatin-process)))

(defun ndlatin-dictionary-title (dictionary)
  (ndlatin-process-open (lookup-dictionary-agent dictionary))
  (cadr (assoc (lookup-dictionary-name dictionary)
               ndlatin-dictionary-table)))

(defun ndlatin-dictionary-search (dictionary query)
  "Search Lat-Eng or Eng-Lat DICTIONARY for QUERY."
  (ndlatin-process-open (lookup-dictionary-agent dictionary))
  (let ((dictname (lookup-dictionary-name dictionary))
        (method   (lookup-query-method query))
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
      (setq output (remove-duplicates output :test 'equal))
      (setq output
            (remove-if ;; remove empty output
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

(defun ndlatin-process-open (agent)
  "Open Latin Words process with name specified by AGENT.
It must be started with command directory."
  (setq ndlatin-program (lookup-agent-location agent))
  (unless ndlatin-program
      (error "ndlatin-program location not specified!"))
  (unless (and (processp ndlatin-process)
               (eq (process-status ndlatin-process) 'run))
    (let ((buffer (or (lookup-open-process-buffer " *ndlatin*")
                      (lookup-temp-buffer))))
      (with-current-buffer buffer
        (let ((default-directory
                (directory-file-name ndlatin-program)))
          (setq ndlatin-process
                (start-process "ndlatin" buffer
                               ndlatin-program)
                ndlatin-status nil
                ndlatin-vars nil)
          (set-process-coding-system ndlatin-process 'utf-8 'utf-8)
          (process-kill-without-query ndlatin-process)
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
