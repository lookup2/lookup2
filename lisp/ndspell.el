;;; ndspell.el --- Lookup spell checker -*- lexical-binding: t -*-
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>
;; Copyright (C) 2009 Lookup Development Team

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Author: Taichi KAWABATA <kawabata.taichi@gmail.com>

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

;; This agent provides spell-checking capabilities for various
;; European languages.  Language can be specified with `location'
;; specification that is actually an language specification option
;; for spell checking program.
;;
;; Example:
;;
;; (setq lookup-search-agents
;;       '(....
;;         (ndspell "--lang=en"
;;                  :charsets '(ascii iso-8859-1))
;;         (ndspell "--lang=ru"
;;                  :title "Russian Spell Checking"
;;                  :charsets '(ascii iso-8859-5))
;;         ....))

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'lookup)
(require 'lookup-utils)

;;;
;;; Customizable variables
;;;

(defgroup ndspell nil
  "Lookup spell checker."
  :group 'lookup-search-agents)

(defcustom ndspell-spell-program "aspell"
  "*Program name of spell."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-spell-options 
  '("-a" "-m" "-C")
  "*Default options for spell checking program."
  :type 'list
  :group 'ndspell)

(defcustom ndspell-spell-dump-options
  '("dump" "master")
  "*Default dictionary dump options for spell checking program."
  :type 'list
  :group 'ndspell)

(defcustom ndspell-shell-program "sh"
  "*Program name for shell program."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-shell-option "-c"
  "*Program option for shell program."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-search-program "grep"
  "*Program name for searching program."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-search-options
  `("-m" ,(number-to-string lookup-max-hits) "-e")
  "*Program options for searching program."
  :group 'ndspell)


;;;
;;; types
;;;

(put 'ndspell :methods '(exact prefix suffix substring wildcard regexp))
(put 'ndspell :priority 'secondary)

;;;
;;; Interface functions
;;;

(defsubst ndspell-command-args (option)
  `(,ndspell-spell-program ,@(if option (split-string option " "))
                           ,@ndspell-spell-options))

(put 'ndspell :list 'ndspell-list)
(defun ndspell-list (agent)
  (when (executable-find ndspell-spell-program)
    (list (lookup-new-dictionary agent ""))))

(put 'ndspell :title 'ndspell-dictionary-title)
(defun ndspell-dictionary-title (dictionary)
  (or (lookup-dictionary-option dictionary :title)
      "Spell Checking"))

;;(put 'ndspell :kill 'ndspell-kill-process)
;;(defun ndspell-kill-process (agent)
;;  (let* ((option (lookup-agent-location agent))
;;         (command-args (ndspell-command-args option)))
;;    (lookup-get-process-kill command-args)))

(put 'ndspell :search 'ndspell-dictionary-search)
(defun ndspell-dictionary-search (dictionary query)
  (let ((string (lookup-query-string query))
        (option (lookup-agent-location
                 (lookup-dictionary-agent dictionary))))
    (mapcar (lambda (word)
              (let ((entry (lookup-new-entry 'dynamic dictionary word)))
                (lookup-put-property entry 'search-word word)
                entry))
            (if (eq (lookup-query-method query) 'exact)
                (ndspell-check-spelling string option)
              (ndspell-search-spelling (lookup-query-to-regexp query)
                                       option)))))

(put 'ndspell :dynamic 'ndspell-dynamic-search)
(defun ndspell-dynamic-search (entry)
  (lookup-with-message "Rechecking"
    (let* ((module (lookup-current-module))
	   (self (lookup-entry-dictionary entry))
	   (word (lookup-get-property entry 'search-word))
	   (query (lookup-new-query 'default word))
	   prio entries search-found)
      (setq entries
	    (mapcar (lambda (dict)
		      (setq prio (lookup-module-dictionary-priority module dict))
		      (when (and (not (eq dict self))
				 (cond ((eq prio t) t)
				       ((eq prio 'secondary) (not search-found))
				       ((eq prio 'supplement) search-found)))
			(lookup-message
			 (format "by %s..." (lookup-dictionary-title dict)))
			(setq entries (lookup-dictionary-search dict query))
			(if entries (setq search-found t))
			entries))
		    (lookup-module-dictionaries module)))
      (setq entries (cl-remove-if
                     (lambda (x) (equal (lookup-entry-type x) 'dynamic))
                     (apply 'append entries)))
      (setq entries (mapcar 'lookup-new-slink entries))
      entries)))


;;;
;;; Internal functions
;;;

(defun ndspell-check-spelling (string option)
  (lookup-with-coding-system 'utf-8
    (let* ((command-args (ndspell-command-args option))
           (output (lookup-get-process-require command-args string)))
      (cond
       ((string= output "") nil)			; empty
       ((eq (aref output 0) ?*) nil)		; match
       ((eq (aref output 0) ?-) nil)		; compound
       ((eq (aref output 0) ?#) nil)		; no match
       ((string-match "^\\+ \\(.*\\)" output)	; root match
        (list (downcase (match-string 1 output))))
       ((string-match "^&[^:]*: " output)		; some candidates
        (split-string (substring output (match-end 0)) "[,\n] ?" t))))))

(defun ndspell-search-spelling (regexp option)
  (with-temp-buffer
    (lookup-with-coding-system 'utf-8
      (call-process 
       ndspell-shell-program nil t nil           ; sh
       ndspell-shell-option                      ; -c
       (mapconcat 'identity
                  `(,ndspell-spell-program       ; aspell
                    ,@(if option 
                      (split-string option " ")) ; --lang=en
                    ,@ndspell-spell-dump-options ; dump master
                    "|"                          ; |
                    ,ndspell-search-program      ; grep
                    ,@ndspell-search-options     ; -e
                    ,regexp) " ")))
    (let ((candidates nil))
      (while (not (bobp))
        (setq candidates (cons (buffer-substring-no-properties
                                (1- (point)) (progn (forward-line -1) (point)))
                               candidates)))
      candidates)))

(provide 'ndspell)

;;; ndspell.el ends here
