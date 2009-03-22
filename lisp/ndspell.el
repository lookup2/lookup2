;;; ndspell.el --- Lookup spell checker
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
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

;;; Code:

(require 'lookup)

;;;
;;; Customizable variables
;;;

(defgroup ndspell nil
  "Lookup spell checker."
  :group 'lookup-search-agents)

(defcustom ndspell-aspell-program "aspell"
  "*Program name of Aspell."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-grep-program "grep"
  "*Program name of grep."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-words-dictionary
  (if (file-exists-p "/usr/dict/words")
      "/usr/dict/words")
  "*Dictionary file."
  :type 'file
  :group 'ndspell)

(defcustom ndspell-dictionary-title "Spell Checker"
  "*Title of ndspell dictionary."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-process-coding-system lookup-process-coding-system
  "*Coding system for Aspell process."
  :type 'symbol
  :group 'ndspell)


;;;
;;; types
;;;

(put 'ndspell :methods '(exact prefix suffix substring wildcard regexp))
(put 'ndspell :priority 'secondary)

;;;
;;; Interface functions
;;;

(put 'ndspell :list 'ndspell-list)
(defun ndspell-list (agent)
  (list (lookup-new-dictionary agent ndspell-aspell-program)))

(put 'ndspell :title ndspell-dictionary-title)

(put 'ndspell :kill 'ndspell-kill-process)

(put 'ndspell :search 'ndspell-dictionary-search)
(defun ndspell-dictionary-search (dictionary query)
  (let ((string (lookup-query-string query)))
    (when (or (not (fboundp 'find-charset-string))
	      (equal (find-charset-string string) '(ascii))
	      (not (find-charset-string string)))
      (mapcar (lambda (word)
		(let ((entry (lookup-new-entry 'dynamic dictionary word)))
		  (lookup-put-property entry 'search-word word)
		  entry))
	      (if (eq (lookup-query-method query) 'exact)
		  (ndspell-check-spelling string)
		(ndspell-search-spelling (lookup-query-to-regexp query)))))))

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
      (setq entries (mapcar 'lookup-new-slink (apply 'append entries)))
      entries)))


;;;
;;; Internal functions
;;;

(defun ndspell-check-spelling (string)
  (let ((output (ndspell-process-require string)))
    (cond
     ((string= output "") nil)			; empty
     ((eq (aref output 0) ?*) nil)		; match
     ((eq (aref output 0) ?-) nil)		; compound
     ((eq (aref output 0) ?#) nil)		; no match
     ((string-match "^\\+ \\(.*\\)" output)	; root match
      (list (downcase (match-string 1 output))))
     ((string-match "^&[^:]*: " output)		; some candidates
      (split-string (substring output (match-end 0)) "[,\n] ?" t)))))

(defun ndspell-search-spelling (regexp)
  (if (file-exists-p ndspell-words-dictionary)
      (with-temp-buffer
        (call-process ndspell-grep-program nil t nil
                      regexp ndspell-words-dictionary)
        (let ((candidates nil))
          (while (not (bobp))
            (setq candidates (cons (buffer-substring-no-properties
                                    (1- (point)) (progn (forward-line -1) (point)))
                                   candidates)))
          candidates))))

;;;
;;; Aspell process
;;;

(defvar ndspell-process nil)

(defun ndspell-get-process ()
  (unless (and ndspell-process (eq (process-status ndspell-process) 'run))
    (if ndspell-process (lookup-process-kill ndspell-process))
    (let ((buffer (lookup-open-process-buffer " *ndspell*")))
      (setq ndspell-process
	    (start-process "ndspell" buffer ndspell-aspell-program
			   "-a" "-m" "-C"))
      (set-process-query-on-exit-flag ndspell-process nil)
      (accept-process-output ndspell-process)
      (let ((coding ndspell-process-coding-system))
	(when coding
	  (set-process-coding-system ndspell-process coding coding)))))
  ndspell-process)

(defun ndspell-process-require (string)
  (lookup-process-require (ndspell-get-process) (concat string "\n") "^\n"))

(defun ndspell-kill-process (agent)
  (if ndspell-process (lookup-process-kill ndspell-process)))

(provide 'ndspell)

;;; ndspell.el ends here
