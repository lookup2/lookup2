;;; ndtut.el --- Lookup tutorial agent
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

(put 'ndtut ':methods '(exact))
(put 'ndtut ':arrange-table '((structure . nil)))

(put 'ndtut ':list 'ndtut-list)
(defun ndtut-list (agent)
  (list (lookup-new-dictionary agent "tutorial")))

(put 'ndtut ':search 'ndtut-dictionary-search)
(defun ndtut-dictionary-search (dictionary query)
  (list (lookup-new-entry 'regular dictionary "")))

(put 'ndtut ':heading 'ndtut-entry-heading)
(defun ndtut-entry-heading (entry)
  "Congratulations!")

(put 'ndtut ':content 'ndtut-entry-content)
(defun ndtut-entry-content (entry)
  "Welcome to the Lookup world!

This is Lookup tutorial program.")

(provide 'ndtut)

;;; ndtut.el ends here
