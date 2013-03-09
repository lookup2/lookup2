;;; ndcookie.el --- Lookup from cookie file -*- lexical-binding: t -*-
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
(require 'cookie1)

(put 'ndcookie :methods lookup-search-methods)
(put 'ndcookie :priority 'supplement)
(put 'ndcookie :arrange-table '((structure . nil)))

(put 'ndcookie :list 'ndcookie-list)
(defun ndcookie-list (agent)
  (mapcar (lambda (name) (lookup-new-dictionary agent name))
	  (directory-files (lookup-agent-location agent) nil "\\.lines\\'")))

(put 'ndcookie :search 'ndcookie-dictionary-search)
(defun ndcookie-dictionary-search (dictionary _query)
  (let ((file (lookup-get-property dictionary 'ndcookie-file)))
    (unless file
      (setq file (expand-file-name (lookup-dictionary-name dictionary)
				   (lookup-agent-location
				    (lookup-dictionary-agent dictionary))))
      (lookup-put-property dictionary 'ndcookie-file file))
    (let ((cookie (cookie file "" "")))
      (while (string-match "\n" cookie)
	(setq cookie (replace-match "\\n" t t cookie)))
      (list (lookup-new-entry 'regular dictionary cookie)))))

(put 'ndcookie :heading 'ndcookie-entry-heading)
(defun ndcookie-entry-heading (entry)
  (let ((cookie (lookup-entry-code entry)))
    (substring cookie 0 (string-match "\\\\" cookie))))

(put 'ndcookie :content 'ndcookie-entry-content)
(defun ndcookie-entry-content (entry)
  (let ((cookie (lookup-entry-code entry)))
    (while (string-match "\\\\n" cookie)
      (setq cookie (replace-match "\n" t t cookie)))
    cookie))

(provide 'ndcookie)

;;; ndcookie.el ends here
