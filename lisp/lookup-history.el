;;; lookup-history.el --- Lookup History mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lookup History Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Construct Buffer
;;;

;;;###autoload
(defun lookup-history-display (module)
  (with-current-buffer (lookup-open-buffer (lookup-history-buffer))
    (lookup-history-mode)
    (let* ((inhibit-read-only t)
	   (history (lookup-module-history module))
	   (list (reverse (lookup-history-stack history)))
	   (num 1) session type)
      (erase-buffer)
      (insert "Tyep `v' to visit session, `q' to leave, `?' for help.\n\n")
      (while list
	(insert (format "%3d: " num))
	(setq num (1+ num) session (car list)
	      type (lookup-session-type session))
	(cond
	 ((eq type 'lookup-search-session)
	  (insert "{" (lookup-query-pattern (lookup-session-query session)) "}"
		  (format " [%d]" (length (lookup-session-entries session))))
	  (let ((entries (lookup-session-entries session))
		(separator " "))
	    (while (and entries (< (current-column) (window-width)))
	      (insert separator)
	      (lookup-entry-heading-insert (car entries))
	      (setq separator "/" entries (cdr entries)))))
	 ((eq type 'lookup-select-session)
	  (insert "Select Session")))
	(insert "\n")
	(setq list (cdr list)))
      (goto-char (point-min))
      (forward-line (1+ (lookup-history-position history))))
    (set-buffer-modified-p nil)
    (lookup-pop-to-buffer (current-buffer))))

;;;
;;; History Mode
;;;

(defconst lookup-history-mode-help
  "Lookup History mode:

`n' - next line  `p' - previous line  `v' - visit session
`r' - return     `q' - leave    `Q' - quit    `R' - restart")

(defvar lookup-history-mode-map nil
  "*Keymap for Lookup History mode.")

(unless lookup-history-mode-map
  (setq lookup-history-mode-map (make-sparse-keymap))
  (set-keymap-parent lookup-history-mode-map lookup-global-map)
  (define-key lookup-history-mode-map " " 'next-line)
  (define-key lookup-history-mode-map "n" 'next-line)
  (define-key lookup-history-mode-map "p" 'previous-line)
  (define-key lookup-history-mode-map "v" 'lookup-history-visit)
  (define-key lookup-history-mode-map "q" 'lookup)
  )

(defvar lookup-history-mode-hook nil
  "*Hook for Lookup History mode.")

(defun lookup-history-mode ()
  "\\{lookup-history-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lookup-history-mode)
  (setq mode-name "History")
  (setq mode-line-buffer-identification '("Lookup:%12b"))
  (setq lookup-mode-help lookup-history-mode-help)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map lookup-history-mode-map)
  (run-hooks 'lookup-history-mode-hook))

;;;
;;; Interactive Commands
;;;

(defun lookup-history-visit ()
  (interactive)
  (beginning-of-line)
  (when (looking-at " *\\([0-9]+\\)")
    (let ((history (lookup-module-history (lookup-current-module)))
	  (position (string-to-int (match-string 1))))
      (lookup-history-set-position history position)
      (lookup-session-display (lookup-history-ref history))
      (princ position))))

(provide 'lookup-history)

;;; lookup-history.el ends here
