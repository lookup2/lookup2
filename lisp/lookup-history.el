;;; lookup-history.el --- Lookup History mode -*- lexical-binding: t -*-
;; Copyright (C) 2000,2009 Lookup Development Team

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

(require 'lookup-types)
(declare-function lookup-get-buffer "lookup")
(declare-function lookup-pop-to-buffer "lookup")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lookup History Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Construct Buffer
;;;

(defun lookup-next-history (&optional arg)
  (interactive "p")
  (let ((session (lookup-history-move lookup-search-history (or arg 1))))
    (lookup-session-display session))
  (princ (lookup-history-position lookup-search-history)))

(defun lookup-previous-history (&optional arg)
  (interactive "p")
  (lookup-next-history (- (or arg 1))))

(defun lookup-history-display (_module)
  "Display lookup histories of MODULE."
  (with-current-buffer (lookup-get-buffer " *Search History*")
    (lookup-history-mode)
    (let* ((inhibit-read-only t)
           (list (reverse (lookup-history-stack lookup-search-history)))
           (num 1) session type)
      (erase-buffer)
      (insert "Type `v' to visit session, `q' to leave, `?' for help.\n\n")
      (dolist (item list)
        (insert (format "%3d: " num))
        (setq num (1+ num) session item
              type nil);(lookup-session-type session))
        (cond
         (t ;(eq type 'lookup-search-query)
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
        (insert "\n"))
      (goto-char (point-min))
      (forward-line (1+ (lookup-history-position lookup-search-history))))
    (set-buffer-modified-p nil)
    (lookup-pop-to-buffer)))

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
  (define-key lookup-history-mode-map "q" 'lookup))

(defvar lookup-history-mode-hook nil
  "*Hook for Lookup History mode.")

(defun lookup-history-mode ()
  "\\{lookup-history-mode-map}."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lookup-history-mode)
  (setq mode-name "History")
  (setq mode-line-buffer-identification '("Lookup:%12b"))
  (setq lookup-help-message lookup-history-mode-help)
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
    (let ((position (string-to-number (match-string 1))))
      (setf (lookup-history-position lookup-search-history) position)
      (lookup-session-display (lookup-history-ref lookup-search-history))
      (princ position))))

(provide 'lookup-history)

;;; lookup-history.el ends here
