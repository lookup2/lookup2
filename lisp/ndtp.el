;;; ndtp.el --- Lookup NDTP client
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
;;; Internal variables
;;;

(defvar ndtp-current-agent nil)
(defvar ndtp-current-dictionary nil)
(defvar ndtp-current-process nil)

(defconst ndtp-process-coding-system
  (if (featurep 'evi-mule) (evi-coding-system 'euc-jp)))

;;;
;;; types
;;;

(put 'ndtp ':methods '(exact prefix suffix))
(put 'ndtp ':gaiji-regexp "<\\(\\(&..?\\|gaiji\\):[^>]*\\)>")
(put 'ndtp ':replace-alist '(("→□\\(#0001\\|<gaiji:z0001>\\)?" . "")))
(put 'ndtp ':reference-pattern
     '("\\(→\\(\\([^<\n]\\|<gaiji:[^>]*>\\)+\\)\\)?<\\([0-9a-f:]+\\)>"
       (or (match-string 1) "(->link)")
       (or (match-string 2) (match-string 4)) 4))

;; ndtp agent:
;;
;;   (ndtp SERVER)
;;
;; SERVER  - host name of NDTP server
;;
;; [option]
;; :service - same as SERVICE above
;; :port    - same as PORT above
;; :account - same as `ndtp-account-name'
;; 
;; [property]
;; ndtp-process - NDTP connection related with agent
;; ndtp-dict    - last used dictionary

(defun 'ndtp-agent-server (agent)
  (let ((server (lookup-agent-location agnet)))
    (if (string-match "\\`//\\([^/]*\\)" server)
	(match-string 1)
      (error "Invalid agent ID" (lookup-agent-id agent)))))

(defun ndtp-agent-service (agent)
  (or (lookup-agent-option agent ':port)
      (lookup-agent-option agent ':service)
      "ndtp"))

(defun ndtp-agent-account (agent)
  (or (lookup-agent-option agent ':account)
      (concat (user-login-name) "@" (system-name))))

(defun ndtp-agent-coding (agent)
  (or (lookup-agent-option agent ':coding)
      ndtp-process-coding-system))

;; ndtp dictionary:
;;
;; CODE  - same as NAME below
;; NAME  - given by server `t' command
;; 
;; [option]
;; :coding  - same as `ndtp-process-coding-system'
;; 
;; [property]
;; ndtp-gaiji - cache buffer for gaiji datas or `disable' if no support

(defun ndtp-dictionary-coding (dictionary)
  (or (lookup-dictionary-option dictionary ':coding t)
      ndtp-process-coding-system))

;; ndtp entry:
;;
;; CODE    - entry specific code (e.g. "2c00:340") by server `Px' command
;; HEADING - given by server `Px' command

(defun ndtp-new-entry (code heading)
  (lookup-new-entry 'regular ndtp-current-dictionary code heading))

;;;
;;; macros
;;;

(put 'ndtp-with-agent 'lisp-indent-function 1)
(defmacro ndtp-with-agent (agent &rest body)
  (` (let ((ndtp-current-agent (, agent))
	   (ndtp-current-process (ndtp-agent-process (, agent))))
       (,@ body))))

(put 'ndtp-with-dictionary 'lisp-indent-function 1)
(defmacro ndtp-with-dictionary (dictionary &rest body)
  (` (ndtp-with-agent (lookup-dictionary-agent (, dictionary))
       (let ((ndtp-current-dictionary (, dictionary)))
	 (unless (eq (, dictionary)
		     (lookup-agent-get-property ndtp-current-agent 'ndtp-dict))
	   ;; 必要なときだけ辞書を select する。
	   ;; 外部プロセスとやりとりするよりこの方が高速だろうし、
	   ;; デバッグのときバッファがごちゃごちゃするのはうざったい。
	   (ndtp-require-select (, dictionary))
	   (lookup-agent-put-property ndtp-current-agent 'ndtp-dict
				      (, dictionary))
	   ;; 辞書毎に文字コードを設定する。
	   (let ((code (ndtp-dictionary-coding (, dictionary))))
	     (when code
	       (set-process-coding-system ndtp-current-process code code))))
	 (,@ body)))))

(defun ndtp-agent-process (agent)
  (let ((process (lookup-agent-get-property agent 'ndtp-process)))
    (unless (and process (eq (process-status process) 'open))
      (if process (lookup-process-kill process))
      (setq process (ndtp-process-open (ndtp-agent-server agent)
				       (ndtp-agent-service agent)))
      ;; 最初に辞書一覧を得るのに文字コードの設定が必要。
      (let ((coding (ndtp-agent-coding agent)))
	(when coding
	  (set-process-coding-system process coding coding)))
      ;; サーバへの接続毎に行なう必要のある処理。
      (let ((ndtp-current-process process))
	(ndtp-process-require (concat "A" (ndtp-agent-account agent)) "\n"))
      (lookup-agent-put-property agent 'ndtp-process process)
      (lookup-agent-put-property agent 'ndtp-dict nil))
    process))

(defun ndtp-agent-kill-process (agent)
  (let ((process (lookup-agent-get-property agent 'ndtp-process)))
    (when process
      (if (eq (process-status process) 'open)
	  (process-send-string process "Q\n"))
      (lookup-process-kill process)
      (lookup-agent-put-property agent 'ndtp-process nil))))


;;;
;;; Interface functions
;;;

(defconst ndtp-dictionary-regexp
  (eval-when-compile
    (let ((_ "[ \t]+") (num "[0-9]+") (title ".+") (name "[^ \t\n]+"))
      (concat "^ *" num _ "\\(" title "\\)" _ "\\(" name "\\)" _
	      num _ num _ num "[ \t]*$"))))

(put 'ndtp ':list 'ndtp-list)
(defun ndtp-list (agent)
  (ndtp-with-agent agent
    (ndtp-process-require "t" "^$\\*\n"
      (lambda (process)
	(let (dicts)
	  (while (re-search-forward ndtp-dictionary-regexp nil t)
	    (setq dicts (cons (lookup-new-dictionary ndtp-current-agent
						     (match-string 2)) dicts)))
	  (nreverse dicts))))))

(put 'ndtp ':kill 'ndtp-agent-kill-process)

(put 'ndtp ':search 'ndtp-dictionary-search)
(defun ndtp-dictionary-search (dictionary query)
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query)))
    ;; build the search command
    (setq string
	  (if (and (featurep 'mule)
		   (memq 'japanese-jisx0208 (find-charset-string string)))
	      (cond ((eq method 'prefix)
		     (concat "Pk" string "*"))
		    ((eq method 'suffix)
		     (concat "PK" (lookup-reverse-string string) "*"))
		    (t (concat "Pk" string)))
	    (cond ((eq method 'prefix)
		   (concat "Pa" string "*"))
		  ((eq method 'suffix)
		   (concat "PA" (lookup-reverse-string string) "*"))
		  (t (concat "Pa" string)))))
    ;; search the pattern
    (ndtp-with-dictionary dictionary
      (ndtp-process-require string "^$[$&N]\n"
	(lambda (process)
	  (let (code heading last-code last-heading entries)
	    (while (re-search-forward "^[^$].*" nil t)
	      (if (not heading)
		  (setq heading (match-string 0))
		(setq code (match-string 0))
		;; 同じエントリが連続していないかチェックする。
		;; これがけっこうあるんだ・・
		(when (or (not (string= code last-code))
			  (not (string= heading last-heading)))
		  (setq entries (cons (ndtp-new-entry code heading) entries))
		  (setq last-code code last-heading heading))
		(setq heading nil)))
	    (nreverse entries)))))))

(put 'ndtp ':gaiji 'ndtp-dictionary-gaiji)
(defun ndtp-dictionary-gaiji (dictionary code)
  (cond
   ((string-match "gaiji:\\([0-9a-z]+\\)" code)
    (list (ndtp-dictionary-font dictionary code)))
   ((string-match "&u:\\([0-9a-f]+\\)" code)
    (vector 'unicode (string-to-int (match-string 1 code) 16)))
   ((string-match "&j2:\\([0-9]+\\)" code)
    (vector 'jisx0212 (string-to-int (match-string 1 code))))
   ((string-match "&g0:\\([0-9]+\\)" code)
    (vector 'gb2312 (string-to-int (match-string 1 code))))
   ((string-match "&c\\([0-7]\\):\\([0-9a-f]+\\)" code)
    (vector (intern (concat "cns" (match-string 1 code)))
	    (string-to-int (match-string 2 code) 16)))))

(put 'ndtp ':font 'ndtp-dictionary-font)
(defun ndtp-dictionary-font (dictionary code)
  (string-match "gaiji:\\([0-9a-z]+\\)" code)
  (setq code (match-string 1 code))
  (let ((buffer (lookup-dictionary-get-property dictionary 'ndtp-gaiji)))
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(if (re-search-forward (format "^$=%s$" code) nil t)
	    (buffer-substring (point) (or (search-forward "$=" nil t)
					  (point-max))))))))

(put 'ndtp ':content 'ndtp-entry-content)
(defun ndtp-entry-content (entry)
  (ndtp-with-dictionary (lookup-entry-dictionary entry)
    (let ((command (concat "S" (lookup-entry-code entry))))
      (substring (ndtp-process-require command "^$$\n") 3))))


;;;
;;; Internal functions
;;;

(defun ndtp-require-select (dictionary)
  (ndtp-process-require (concat "L" (lookup-dictionary-name dictionary)) "\n")
  (and
   lookup-enable-gaiji
   (lookup-gaiji-glyph-possible-p)
   (ndtp-process-require "XL16" "^$.\n")
   (let ((buffer (lookup-dictionary-get-property dictionary 'ndtp-gaiji)))
     (unless buffer
       (if (not (string-match "16" (ndtp-process-require "XI" "^$[$N?]\n")))
	   (setq buffer 'disable)
	 (setq buffer (generate-new-buffer
		       (format " *ndtp gaiji table for %s*"
			       (lookup-dictionary-id dictionary))))
	 (ndtp-process-require "XL16" "^$.\n")
	 (with-current-buffer buffer
	   (insert (ndtp-process-require "XB" "^$$\n"))))
       (lookup-dictionary-put-property dictionary 'ndtp-gaiji buffer)))))

;;;
;;; ndtp process
;;;

(defun ndtp-process-open (server service)
  (lookup-proceeding-message (format "connecting to %s..." server))
  (let* ((buffer (lookup-open-process-buffer (concat " *ndtp+" server "*")))
	 (process (open-network-stream "ndtp" buffer server service)))
    (process-kill-without-query process)
    process))

(put 'ndtp-process-require 'lisp-indent-function 2)
(defun ndtp-process-require (command separator &optional filter)
  (lookup-process-require ndtp-current-process (concat command "\n")
			  separator filter))

(provide 'ndtp)

;;; ndtp.el ends here
