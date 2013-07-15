;;; ndwnj.el --- search inferface for Japanese WordNet -*- lexical-binding: t -*-
;; Copyright (C) 2009  Kazuhiro Ito <kzhr@d1.dion.ne.jp>

;; Author: Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;; Modified by: Kawabata Taichi

;; ndwnj.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; ndwnj.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; Specify the location of wnj.db in lookup-search-agents variable.
;; e.g.
;; (setq lookup-search-agents
;;       '(...
;;         (ndwnj "/path/to/wnjpn.db")
;;         ...))

;;; Code:

(eval-when-compile (require 'cl))
(require 'lookup)

(defconst ndwnj-version "0.1")

;;;
;;; Customizable variables
;;;

(defgroup ndwnj nil
  "Lookup ndwnj interface."
  :group 'lookup-agents)

(defcustom ndwnj-program-name "sqlite3"
  "*Program name of sqlite3."
  :type 'string
  :group 'ndwnj)

(defcustom ndwnj-program-arguments '("-interactive")
  "*A list of arguments for sqlite3."
  :type '(repeat (string :tag "option"))
  :group 'ndwnj)

(defcustom ndwnj-prompt-string "sqlite> "
  "*Prompt string of sqlite3."
  :type 'string
  :group 'ndwnj)

(defcustom ndwnj-process-coding-system
  'utf-8
  "*Coding system for sqlite3 process."
  :type 'symbol
  :group 'ndwnj)


;;;
;;; Internal variables
;;;


;;;
;:: types
;;;

(defun ndwnj-agent-coding (agent)
  (or (lookup-agent-option agent :coding)
      ndwnj-process-coding-system))

(defun ndwnj-get-process (dictionary)
  "Get sqlite3 process."
  (let* ((agent   (lookup-dictionary-agent dictionary))
         (file    (lookup-agent-location agent))
         (coding  (ndwnj-agent-coding agent)))
    (ndwnj-process file coding)))

(put 'ndwnj :methods '(exact prefix suffix substring wild))
(put 'ndwnj :reference-pattern '("<\\([0-9]+-[a-z]+\\):\\([^>]+\\)>" 2 2 1))
(put 'ndwnj :arranges
     '((structure lookup-arrange-structure
		  ndwnj-arrange-headings
		  ndwnj-arrange-remove-dups)
       (reference lookup-arrange-references)
       (fill      lookup-arrange-fill-lines)))


;;;
;:: Interface functions
;;;

(put 'ndwnj :list 'ndwnj-list)
(defun ndwnj-list (agent)
  (let* ((location   (lookup-agent-location agent)))
    (when (file-exists-p location)
      (list (lookup-new-dictionary agent "")))))

(put 'ndwnj :title 'ndwnj-title)
(defun ndwnj-title (_dictionary)
  "日本語 WordNet")

(put 'ndwnj :clear 'ndwnj-clear)
(defun ndwnj-clear (dictionary)
  (let ((process (ndwnj-get-process dictionary)))
    (when process
      (when (eq (process-status process) 'run)
	(process-send-string process ".exit\n"))
      (lookup-process-kill process))))

(put 'ndwnj :search 'ndwnj-dictionary-search)
(defun ndwnj-dictionary-search (dictionary query)
  (let* ((method  (lookup-query-method query))
         (string  (ndwnj-escape-string
                   (ndwnj-normalize-string
                    (lookup-query-string query))))
         (process (ndwnj-get-process dictionary))
         (cmd (case method
                ('exact     (concat "= '" string "'"))
                ('prefix    (concat "GLOB '" string "*'"))
                ('suffix    (concat "GLOB '*" string "'"))
                ('substring (concat "GLOB '*" string "*'"))
                ('wild      (concat "GLOB '" string "'")))))
    (loop for (code heading) in (ndwnj-search process cmd)
          collect
          (lookup-new-entry
           'regular dictionary code heading))))

(put 'ndwnj :content 'ndwnj-dictionary-content)
(defun ndwnj-dictionary-content (entry)
  (let* ((dictionary (lookup-entry-dictionary entry))
         (process    (ndwnj-get-process dictionary))
         (code       (lookup-entry-code entry))
         (heading    (concat (lookup-entry-code entry)
                             " "
                             (lookup-entry-heading entry)))
         (def        (ndwnj-get-definition process code))
	 (ex         (ndwnj-get-examples process code))
         (word       (ndwnj-get-words process code))
         (link       (ndwnj-get-links process code))
	 (index 0))
    (when ex
      (while (setq index (and (string-match ": \\(.+\\)$" ex index)
			      (match-end 0)))
	(setq def (replace-regexp-in-string
		   (concat "; \"" (regexp-quote (match-string 1 ex)) "\"")
		   "" def t t))))
    (concat heading "\n" def "\n"
	    (and ex (concat "\n" ex "\n")) "\n" word "\n" link "\n")))


;;;
;;; Arrange functions
;;;

(defun ndwnj-arrange-remove-dups (_entry)
  (goto-char (point-max))
  (beginning-of-line)
  (while (null (bobp))
    (let ((start (point))
	  (end (progn (end-of-line) (point))))
      (forward-line -1)
      (when (and (null (eq start end))
		 (looking-at (regexp-quote (buffer-substring start end))))
	(delete-region start (min (1+ end) (point-max)))))))

(defun ndwnj-arrange-headings (_entry)
  (while (re-search-forward
	  "^\\( *\\)\\(\\(\\[[^][]+\\]\\)\\|\\([a-zA-Z]+:\\)\\)" nil t)
    (cond
     ((match-beginning 3)
      (lookup-make-region-heading (match-beginning 3) (match-end 3) 2))
     ((match-beginning 4)
      (lookup-make-region-heading (match-beginning 4) (match-end 4) 3)))))


;;;
;;; Normalizers
;;;

(defun ndwnj-normalize-string (string)
    (replace-regexp-in-string " " "_" string))

(defun ndwnj-escape-string (string)
  (replace-regexp-in-string "'" "''" string))


;;;
;;; Internal functions
;;;

(defun ndwnj-process (file coding)
  (unless (file-exists-p file) (error "ndwnj: file %s not found." file))
  (let* ((coding-system-for-read coding)
         (coding-system-for-write coding)
         (process-environment
          (append '("LANG=en_US.utf-8")
                  process-environment)))
    (lookup-get-process (append (list ndwnj-program-name)
                                ndwnj-program-arguments
                                (list (file-truename file))))))

(defun ndwnj-require (process string &optional filter)
  "Send string to sqlite3 process and return output. Cf. `lookup-process-require'"
  (declare (indent 1))
  (let ((lookup-process-output-separator-lines 0))
    (lookup-process-require process
			    (concat string "\n")
			    (concat "^" ndwnj-prompt-string)
                            filter)))

(defun ndwnj-search (process command)
  (ndwnj-require process
    (concat "SELECT synset, name, pos_def.def "
            "FROM synset, pos_def ON synset.pos = pos_def.pos "
            "WHERE synset IN "
            "(SELECT synset FROM sense WHERE wordid IN "
            "(SELECT wordid FROM word WHERE lemma "
            command
            ")) AND pos_def.lang = 'jpn' "
            (when (and (numberp lookup-max-hits)
                       (> lookup-max-hits 0))
              (concat "LIMIT " (number-to-string lookup-max-hits)))
            ";")
    (lambda (_process)
      (let (entries)
        (while (re-search-forward "^\\(.+\\)|\\(.+\\)|\\(.+\\)$" nil t)
          (lookup-debug-message "hit! buffer=%s" (match-string 0))
          (push (list (match-string 1)
                      (concat (match-string 2)
                              " (" (match-string 3) ")")) entries))
        (nreverse entries)))))

(defun ndwnj-get-words (process code)
  (ndwnj-require process
    (concat "SELECT lemma, lang FROM word WHERE wordid IN "
	    "(SELECT wordid FROM sense WHERE synset = '"
	    (ndwnj-escape-string code)
	    "') ORDER BY lang DESC;")
    (lambda (_process)
      (let (lemma lang elt (results (make-hash-table :test 'equal))  result)
	(while (re-search-forward "^\\(.+\\)|\\(.+\\)$" nil t)
	  (setq lemma (match-string 1)
		lang (match-string 2))
	  (setq elt (concat (gethash lang results) ", " lemma))
	  (puthash lang elt results))
	(maphash (lambda (x y)
                   (callf concat result "  " x ": " (substring y 2) "\n"))
                 results)
	result))))

(defun ndwnj-get-examples (process code)
  (ndwnj-require process
    (concat "SELECT lang, def, sid FROM synset_ex WHERE synset = '"
	    (ndwnj-escape-string code)
	    "' ORDER BY sid ASC, lang ASC;")
    (lambda (_process)
      (let (results)
	(while (re-search-forward "^\\(.+\\)|\\(.+\\)|.+$" nil t)
	  (push (concat "  " (match-string 1) ": " (match-string 2)) results))
        (and results
	     (mapconcat 'identity (nreverse results) "\n"))))))

(defun ndwnj-get-definition (process code)
  (ndwnj-require process
    (concat "SELECT lang, def FROM synset_def WHERE synset = '"
	    (ndwnj-escape-string code)
	    "' ORDER BY lang;")
    (lambda (_process)
      (let (results)
        (while (re-search-forward "^\\(.+\\)|\\(.+\\)$" nil t)
          (push (concat (match-string 1) ": " (match-string 2)) results))
        (mapconcat 'identity (nreverse results) "\n")))))

(defun ndwnj-get-links (process code)
  (ndwnj-require process
    (concat "SELECT synlink.synset2, synset.name, link_def.def "
	    "FROM synlink, synset ON synlink.synset2 = synset.synset "
	    ", link_def ON synlink.link = link_def.link "
	    "WHERE synlink.synset1 = '"
	    (ndwnj-escape-string code)
	    "' ORDER BY synlink.link DESC, synset.name;")
    (lambda (_process)
      (let (target name syn elt (results (make-hash-table :test 'equal)) result)
	(while (re-search-forward "^\\(.+\\)|\\(.+\\)|\\(.+\\)$" nil t)
	  (setq target (match-string 1)
		name (match-string 2)
		syn (match-string 3)
                elt (concat (gethash syn results)
                            " <" target ":" name ">"))
	  (puthash syn elt results))
	(maphash (lambda (x y)
                   (callf concat result " [" x "]" y "\n"))
                 results)
	result))))

(provide 'ndwnj)

;;; ndwnj.el ends here
