;;; ndsrd.el --- search agent for 小学館『ランダムハウス英語辞典』
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

(defgroup ndsrd nil
  "Lookup csrd interface."
  :group 'lookup-search-agents)

(defcustom ndsrd-program-name "csrd"
  "*Command name of `csrd'."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-dictionary-title "ランダムハウス英語辞典"
  "*Title of ndsrd dictionary."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-process-coding-system lookup-process-coding-system
  "*Coding system for csrd process."
  :type 'symbol
  :group 'ndsrd)


;;;
;;; types
;;;

(put 'ndsrd :methods '(exact prefix suffix substring wildcard keyword))
(put 'ndsrd :reference-pattern '("→\\([A-Z]*\\)" 0 1 lookup-dynamic-search))
(put 'ndsrd :stemmer 'stem-english)


;;;
;;; Interface functions
;;;

(put 'ndsrd :list 'ndsrd-list)
(defun ndsrd-list (agent)
  (list (lookup-new-dictionary agent ndsrd-program-name)))

(put 'ndsrd :title ndsrd-dictionary-title)

(put 'ndsrd :search 'ndsrd-dictionary-search)
(defun ndsrd-dictionary-search (dictionary query)
  (with-current-buffer (get-buffer-create " *ndsrd*")
    (goto-char (point-max))
    (let ((opts (lookup-get-property dictionary 'ndsrd-opts)))
      (unless opts
	(let* ((agent (lookup-dictionary-agent dictionary))
	       (dir (expand-file-name (lookup-agent-location agent)))
	       (gai (lookup-agent-option agent :gai))
	       (fmt (lookup-agent-option agent :fmt)))
	  (setq opts '("-a"))
	  (if gai (setq opts (cons (concat "-g" (expand-file-name gai)) opts)))
	  (if fmt (setq opts (cons (concat "-f" (expand-file-name fmt)) opts)))
	  (setq opts (cons (concat "-d" dir) opts))
	  (lookup-put-property dictionary 'ndsrd-opts opts)))
      (setq opts (append opts (if (eq (lookup-query-method query) 'keyword)
				  (list "-i" (lookup-query-string query))
				(list (lookup-query-to-wildcard query)))))
      (if lookup-enable-debug
	  (insert "> " ndsrd-program-name " " (mapconcat 'eval opts " ") "\n"))
      (goto-char
       (prog1 (point)
	 (lookup-with-coding-system ndsrd-process-coding-system
	   (apply 'call-process ndsrd-program-name nil t nil opts)))))
    (let (start heading entry entries)
      (while (looking-at "□ \\([^\[\n]*\\)")
	(setq start (point) heading (match-string 1))
	(while (string-match "[・´｀/]\\| $" heading)
	  (setq heading (replace-match "" t t heading)))
	(forward-line)
	(if (re-search-forward "^□" nil 0)
	    (goto-char (match-beginning 0)))
	(setq entry (lookup-new-entry 'regular dictionary heading heading))
	(lookup-put-property entry 'ndsrd-content
			     (buffer-substring start (point)))
	(setq entries (cons entry entries)))
      (if (not lookup-enable-debug) (kill-buffer (current-buffer)))
      (nreverse entries))))

(put 'ndsrd :content 'ndsrd-entry-content)
(defun ndsrd-entry-content (entry)
  (or (lookup-get-property entry 'ndsrd-content) "(forgot)"))

(provide 'ndsrd)

;;; ndsrd.el ends here
