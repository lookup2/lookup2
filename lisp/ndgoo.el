;;; ndgoo.el --- Lookup dictionary.goo.ne.jp interface
;; Copyright (C) 2001 Keisuke Nishida <knishida@ring.gr.jp>

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

(defvar ndgoo-url "http://dictionary.goo.ne.jp")

(defvar ndgoo-action "/cgi-bin/dict_search.cgi?MT=%s&sw=%s")

(defvar ndgoo-dictionary-table
  '(("je" "GOO/英和辞典" 0)
    ("ej" "GOO/和英辞典" 1)
    ("jp" "GOO/国語辞典" 2)
    ("nw" "GOO/新語辞典" 3)))

(put 'ndgoo :methods '(exact))

;;;
;;; Interface functions
;;;

(put 'ndgoo :list 'ndgoo-list)
(defun ndgoo-list (agent)
  (mapcar (lambda (e) (lookup-new-dictionary agent (car e)))
	  ndgoo-dictionary-table))

(put 'ndgoo :title 'ndgoo-dictionary-title)
(defun ndgoo-dictionary-title (dictionary)
  (cadr (assoc (lookup-dictionary-name dictionary)
	       ndgoo-dictionary-table)))

(put 'ndgoo :search 'ndgoo-dictionary-search)
(defun ndgoo-dictionary-search (dictionary query)
  (with-temp-buffer
    (call-process "wget" nil t nil "-q" "-O" "-"
		  (format (concat ndgoo-url ndgoo-action)
			  (lookup-query-string query)
			  (caddr (assoc (lookup-dictionary-name dictionary)
					ndgoo-dictionary-table))))
    ;; delete headers and footers
    (goto-char (point-min))
    (search-forward "<!-- ej_res1 -->")
    (delete-region (point-min) (point))
    (search-forward "<!-- ej_res1 -->")
    (delete-region (point) (point-max))
    ;; extract entries
    (goto-char (point-min))
    (let ((entries nil))
      (while (search-forward "nowrap" nil t)
	(re-search-forward "<a href=\"\\([^\"]*\\)\">\\([^<]*\\)</a>")
	(setq entries (cons (lookup-new-entry 'regular dictionary
					      (match-string 1)
					      (match-string 2))
			    entries)))
      (nreverse entries))))

(put 'ndgoo :content 'ndgoo-entry-content)
(defun ndgoo-entry-content (entry)
  (with-temp-buffer
    (call-process "wget" nil t nil "-q" "-O" "-"
		  (concat ndgoo-url (lookup-entry-code entry)))
    (buffer-string)))

(provide 'ndgoo)

;;; ndgoo.el ends here
