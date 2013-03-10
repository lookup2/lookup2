;;; ndict.el --- Lookup DICT client -*- lexical-binding: t -*-
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

(defconst ndict-version "1.0")

;;;
;;; Internal variables
;;;

(defvar ndict-current-agent nil)
(defvar ndict-current-dictionary nil)
(defvar ndict-current-process nil)
(defvar ndict-match-exact nil)

(defconst ndict-system-info-alist
  '(;; dictd-1.4.x or later
    (dictd
     (methods . ((exact . "exact") (prefix . "prefix") (suffix . "suffix")
		 (substring . "substring") (regexp . "re"))))
    ;; minimam feature defined in RFC2229
    (t
     (methods . ((exact . "exact") (prefix . "prefix"))))))

(defvar ndict-process-coding-system 'mule-utf-8)

;;;
;;; types
;;;

(put 'ndict :arrange-table '((fill lookup-arrange-nofill)))

;; ndict agent:
;;
;;   "ndict://[USER@]SERVER[:PORT]/"
;;
;; SERVER  - host name of DICT server
;;
;; [option]
;; :service - same as SERVICE above
;; :port    - same as PORT above
;; :coding  - same as `ndict-process-coding-system'
;; :auth    - cons of username and authentication-string
;; :method-table - table to transfer a search method from Lookup internal
;;                 to DICT server defined
;; 
;; [property]
;; ndict-process - DICT connection related with agent

(defalias 'ndict-agent-server 'lookup-agent-location)

(defun ndict-agent-service (agent)
  (or (lookup-agent-option agent :port)
      (lookup-agent-option agent :service)
      "dict"))

(defun ndict-agent-coding (agent)
  (or (lookup-agent-option agent :coding)
      ndict-process-coding-system))

(defun ndict-agent-auth (agent)
  (lookup-agent-option agent :auth))

(defun ndict-agent-method-table (agent)
  (lookup-agent-option agent :method-table))

;; ndict dictionary:
;;
;; NAME  - given by server `SHOW DB' command
;; TITLE - given by server `SHOW DB' command

(defun ndict-make-dictionary (name title)
  (let ((dictionary
         (lookup-new-dictionary ndict-current-agent name)))
    (setf (lookup-dictionary-title dictionary) title) ;; title))
    dictionary))

;; ndict entry:
;;
;; NAME    - returned value of `MATCH' command
;; HEADING - given by server `MATCH' command

(defsubst ndict-new-entry (name heading)
  (lookup-new-entry 'regular ndict-current-dictionary name heading))

;;;
;;; macros
;;;

(defmacro ndict-with-agent (agent &rest body)
  (declare (indent 1))
  `(let ((ndict-current-agent ,agent)
	   (ndict-current-process (ndict-agent-process ,agent)))
     ,@body))

(defmacro ndict-with-dictionary (dictionary &rest body)
  (declare (indent 1))
  `(ndict-with-agent (lookup-dictionary-agent ,dictionary)
     (let ((ndict-current-dictionary ,dictionary))
       ,@body)))

(defun ndict-agent-process (agent)
  (let ((process (lookup-get-property agent 'ndict-process)))
    (unless (and process (eq (process-status process) 'open))
      (if process (lookup-process-kill process))
      (setq process (ndict-process-open (ndict-agent-server agent)
					(ndict-agent-service agent)))
      ;; initial setup
      (let ((coding (ndict-agent-coding agent)))
        (set-process-coding-system process coding coding))
      (let ((ndict-current-process process)
	    (auth (ndict-agent-auth agent)))
	(ndict-process-require (concat "CLIENT ndict " ndict-version))
	(when auth
	  (ndict-process-require (concat "AUTH" (car auth) (cdr auth)))))
      (lookup-put-property agent 'ndict-process process))
    process))

(defun ndict-agent-kill-process (agent)
  (let ((process (lookup-get-property agent 'ndict-process)))
    (when process
      (if (eq (process-status process) 'open)
	  (process-send-string process "QUIT\n"))
      (lookup-process-kill process))))


;;;
;;; Interface functions
;;;

(put 'ndict :list 'ndict-list)
(defun ndict-list (agent)
  (ndict-with-agent agent
    ;; get server information
    (let* ((server (ndict-process-require "SHOW SERVER"
		     (lambda (_process)
		       (forward-line)
		       (buffer-substring (point) (line-end-position)))))
	   (system (if (string-match "^dictd " server) 'dictd t))
	   (table (lookup-assq-get ndict-system-info-alist system)))
      ;; set method-table
      (let* ((method-table (lookup-assq-get table 'methods)))
	(setf (lookup-agent-option agent :method-table) method-table)))
    ;; get dictionary list
    (ndict-process-require "SHOW DB"
      (lambda (_process)
	(let (name title dicts)
	  (when (looking-at "110")
	    (forward-line)
	    (narrow-to-region (point) (re-search-forward "^\\.$"))
	    (while (re-search-backward "^\\([^ ]+\\) \\(.*\\)" nil t)
	      (setq name (match-string 1) title (read (match-string 2)))
	      (setq dicts (cons (ndict-make-dictionary name title) dicts)))
	    dicts))))))

(put 'ndict :methods 'ndict-methods)
(defun ndict-methods (dictionary)
  (let ((agent (lookup-dictionary-agent dictionary)))
    (mapcar 'car (ndict-agent-method-table agent))))

(put 'ndict :kill 'ndict-kill)
(defun ndict-clear (agent)
  (ndict-agent-kill-process agent))

(put 'ndict :search 'ndict-dictionary-search)
(defun ndict-dictionary-search (dictionary query)
  (ndict-with-dictionary dictionary
    (let* ((method (lookup-query-method query))
	   (string (lookup-query-string query))
	   (db (lookup-dictionary-name dictionary))
	   (table (ndict-agent-method-table ndict-current-agent))
	   (strategy (lookup-assq-get table method))
	   (ndict-match-exact (if (eq method 'exact) (concat "^" string "$")))
           (string-coding-systems (find-coding-systems-string string)))
      (when (or (eq 'undecided (car string-coding-systems))
                (memq (ndict-agent-coding ndict-current-agent) string-coding-systems))
        (ndict-process-require (format "MATCH %s %s '%s'" db strategy string)
          (lambda (_process)
            (when (looking-at "152")
              (forward-line)
              (narrow-to-region (point) (re-search-forward "^\\.$"))
              (let ((case-fold-search t) name heading entries)
                (while (re-search-backward "^[^ ]+ \\(.*\\)" nil t)
                  (setq name (match-string 1) heading (read name))
                  (if (or (not ndict-match-exact)
                          (string-match ndict-match-exact heading))
                      (setq entries (cons (ndict-new-entry name heading)
                                          entries))))
                entries))))))))

(put 'ndict :content 'ndict-entry-content)
(defun ndict-entry-content (entry)
  (ndict-with-dictionary (lookup-entry-dictionary entry)
    (let ((db (lookup-dictionary-name ndict-current-dictionary))
	  (word (lookup-entry-code entry)))
      (ndict-process-require (format "DEFINE %s %s" db word)
	(lambda (_process)
	  (when (looking-at "150")
	    (forward-line 2)
	    (buffer-substring (point) (progn (re-search-forward "^\\.$")
					     (match-beginning 0)))))))))


;;;
;;; ndict process
;;;

(defun ndict-process-open (server service)
  (lookup-message (format "connecting to %s..." server))
  (let* ((buffer (lookup-open-process-buffer (concat " *ndict+" server "*")))
	 (process (open-network-stream "ndict" buffer server service)))
    (accept-process-output process)
    (set-process-query-on-exit-flag process nil)
    process))

(defun ndict-process-require (command &optional filter)
  (declare (indent 1))
  (lookup-process-require ndict-current-process (concat command "\n")
			  "^[245][0-8][0-9] .*\n" filter))

(provide 'ndict)

;;; ndict.el ends here
