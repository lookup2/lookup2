;;; evi-mule.el --- integrate some Mule futures
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

(defconst evi-mule-version
  (if (boundp 'mule-version)
      mule-version
    (if (and (featurep 'xemacs) (featurep 'mule))
	"3.0")))

(defconst evi-coding-system-table
  (if (string< evi-mule-version "3.0")
      '((no-conversion . *noconv*)
	(binary        . *noconv*)
	(emacs-mule    . *intern*)
	(junet         . *junet*)
	(sjis          . *sjis*)
	(euc-jp        . *euc-japan*)
	(euc-jp-dos    . *euc-japan*dos))))

(defun evi-coding-system (key)
  (or (cdr (assq key evi-coding-system-table)) key))

(if (string< evi-mule-version "3.0")
    (defsubst evi-define-program-coding-system (pattern val)
      (define-program-coding-system nil pattern val))
  (defsubst evi-define-program-coding-system (pattern val)
    (add-to-list 'process-coding-system-alist (cons pattern val))))

(provide 'evi-mule)

;;; evi-mule.el ends here
