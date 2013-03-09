;;; ndtut.el --- Lookup tutorial agent -*- lexical-binding: t -*-
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>
;; Copyright (C) 2009 Lookup Development Team

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

(put 'ndtut :methods '(exact))
(put 'ndtut :arrange-table '((structure . nil)))

(put 'ndtut :list 'ndtut-list)
(defun ndtut-list (agent)
  (list (lookup-new-dictionary agent "tutorial")))

(put 'ndtut :search 'ndtut-dictionary-search)
(defun ndtut-dictionary-search (dictionary _query)
  (list (lookup-new-entry 'regular dictionary "")))

(put 'ndtut :heading 'ndtut-entry-heading)
(defun ndtut-entry-heading (_entry)
  "Welcome to Lookup!")

(put 'ndtut :content 'ndtut-entry-content)
(defun ndtut-entry-content (_entry)
  "Display tutorial for dummy ENTRY."
  "Welcome to Lookup!

This is Lookup tutorial program.

If you see this message, you may have not yet set the variable
`lookup-search-agents'.

Please read the manual, identify your dictionary resources and
set up variable `lookup-search-agents'.

Thank you for trying Lookup!!
")

(provide 'ndtut)

;;; ndtut.el ends here
