;;; lookup.el --- Search interface to electronic dictionaries
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

(require 'evi)
(require 'lookup-vars)
(require 'lookup-utils)
(require 'lookup-types)


;;;;;;;;;;;;;;;;;;;;
;; Top-level
;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun lookup-version (arg)
  "Display the version string of Lookup.
With prefix argument, insert string at point."
  (interactive "P")
  (let ((version (concat "Lookup " lookup-version)))
    (if arg (insert version) (message version))))

;;;###autoload
(defun lookup (&optional module)
  "Start Lookup and display the list of your dictionaries.
If you have already started lookup, display the last status of buffers.
With prefix arg, you can choose which module you use."
  (interactive (list (if current-prefix-arg (lookup-input-module))))
  (setq module (or module (if lookup-last-session
			      (lookup-session-module lookup-last-session)
			    (lookup-default-module))))
  (let ((position (lookup-history-position lookup-search-history)))
    (if (> position 0)
	(lookup-session-display (lookup-history-ref lookup-search-history
						    position))
      (lookup-select-session module))))

(defun lookup-kill ()
  "Force Lookup to be quiet when you exit Emacs.
This can be used when you cannot finish Emacs because of an error of Lookup."
  (interactive)
  (remove-hook 'kill-emacs-hook 'lookup-exit)
  (message "OK, now you can exit Emacs"))

(defun lookup-debug ()
  "Toggle Lookup debug mode."
  (interactive)
  (setq lookup-debug-mode (not lookup-debug-mode))
  (setq debug-on-error lookup-debug-mode)
  (message (if lookup-debug-mode
	       "Lookup debug enabled"
	     "Lookup debug disabled")))

(defun lookup-initialize ()
  (load lookup-init-file t)
  (if lookup-cache-file (lookup-restore-cache lookup-cache-file))
  (run-hooks 'lookup-load-hook)
  (setq lookup-search-history (lookup-new-history))
  (lookup-init-gaiji-functions)
  (lookup-init-complement-autoload))

(defun lookup-init-complement-autoload ()
  (if lookup-complement-directory
      (load (expand-file-name "loaddef.el" lookup-complement-directory))
    (load "loaddef.el"))
  (lookup-foreach
   (lambda (pair)
     (let ((dicts (lookup-dictionary-alist)) dict)
       (while dicts
	 (if (string-match (car pair) (caar dicts))
	     (setq dict (cdar dicts) dicts nil)
	   (setq dicts (cdr dicts))))
       (if dict
	   (lookup-assoc-set 'lookup-complement-alist
			     (lookup-dictionary-id dict) (cdr pair)))))
   lookup-complement-autoload-alist))


;;;;;;;;;;;;;;;;;;;;
;; Search Commands
;;;;;;;;;;;;;;;;;;;;

(defun lookup-pattern-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(pattern (lookup-input-pattern)))
    (list pattern module)))

;;;###autoload
(defun lookup-pattern (pattern &optional module)
  "Search for the PATTERN."
  (interactive (lookup-pattern-input))
  (lookup-search-pattern (or module (lookup-default-module)) pattern))

;;;###autoload
(defun lookup-pattern-full-screen (pattern &optional module)
  "Search for the PATTERN in full screen.
See `lookup-pattern' for details."
  (interactive (lookup-pattern-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-pattern pattern module)))

;;;###autoload
(defun lookup-pattern-other-frame (pattern &optional module)
  "Search for the PATTERN in another frame.
See `lookup-pattern' for details."
  (interactive (lookup-pattern-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-pattern pattern module)))

(defun lookup-word-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(word (lookup-current-word)))
    (list word module)))

;;;###autoload
(defun lookup-word (word &optional module)
  "Search for the word near the cursor."
  (interactive (lookup-word-input))
  (let ((lookup-search-method lookup-default-method))
    (lookup-pattern word module)))

;;;###autoload
(defun lookup-word-full-screen (word &optional module)
  "Search for the word near the cursor in full screen.
See `lookup-word' for details."
  (interactive (lookup-word-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-word word module)))

;;;###autoload
(defun lookup-word-other-frame (word &optional module)
  "Search for the word near the cursor in another frame.
See `lookup-word' for details."
  (interactive (lookup-word-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-word word module)))

(defun lookup-region-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(start (mark)) (end (point)) tmp)
    (if (> start end) (setq tmp start start end end tmp))
    (list start end module)))

;;;###autoload
(defun lookup-region (start end &optional module)
  "Search for the region."
  (interactive (lookup-region-input))
  (lookup-word (buffer-substring-no-properties start end) module))

;;;###autoload
(defun lookup-region-full-screen (start end &optional module)
  "Search for the region in full screen.
See `lookup-region' for details."
  (interactive (lookup-region-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-region start end module)))

;;;###autoload
(defun lookup-region-other-frame (start end &optional module)
  "Search for the region in another frame.
See `lookup-region' for details."
  (interactive (lookup-region-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-region start end module)))

;;;###autoload
(defun lookup-selection (click)
  "Search for the mouse's selection."
  (interactive "e")
  (lookup-word (current-kill 0 t)))

;;;###autoload
(defun lookup-selection-full-screen (click)
  "Search for the mouse's selection in full screen.
See `lookup-selection' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-selection click)))

;;;###autoload
(defun lookup-selection-other-frame (click)
  "Search for the mouse's selection in another frame.
See `lookup-selection' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-selection click)))

;;;###autoload
(defun lookup-secondary (click)
  "Search for the mouse's secondary selection."
  (interactive "e")
  (call-interactively 'mouse-drag-secondary)
  (let ((start (overlay-start mouse-secondary-overlay))
	(end (overlay-end mouse-secondary-overlay)))
    (unless (eq start end)
      (with-current-buffer (window-buffer (posn-window (event-start click)))
	(unwind-protect
	    (lookup-word (buffer-substring-no-properties start end))
	  (delete-overlay mouse-secondary-overlay))))))

;;;###autoload
(defun lookup-secondary-full-screen (click)
  "Search for the mouse's secondary selection in full screen.
See `lookup-secondary' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-secondary click)))

;;;###autoload
(defun lookup-secondary-other-frame (click)
  "Search for the mouse's secondary selection in another frame.
See `lookup-secondary' for details."
  (interactive "e")
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-secondary click)))


;;;;;;;;;;;;;;;;;;;;
;; Global Commands
;;;;;;;;;;;;;;;;;;;;

(defvar lookup-global-map nil)

(unless lookup-global-map
  (setq lookup-global-map (make-sparse-keymap))
  (define-key lookup-global-map "\en" 'lookup-next-history)
  (define-key lookup-global-map "\ep" 'lookup-previous-history)
;  (define-key lookup-global-map "\es" 'lookup-isearch-history-forward)
;  (define-key lookup-global-map "\er" 'lookup-isearch-history-backward)
  (define-key lookup-global-map "\ef" 'lookup-forward-module)
  (define-key lookup-global-map "\eb" 'lookup-backward-module)
  (define-key lookup-global-map "B" 'lookup-list-bookmarks)
  (define-key lookup-global-map "H" 'lookup-list-history)
  (define-key lookup-global-map "S" 'lookup-record-display)
  (define-key lookup-global-map "f" 'lookup-find-pattern)
  (define-key lookup-global-map "o" 'lookup-open-window)
  (define-key lookup-global-map "r" 'lookup-return)
  (define-key lookup-global-map "q" 'lookup-suspend)
  (define-key lookup-global-map "Q" 'lookup-exit)
  (define-key lookup-global-map "R" 'lookup-restart)
  (define-key lookup-global-map "?" 'lookup-help))

(defun lookup-create-module (name)
  (interactive "sModule name: ")
  (if (lookup-get-module name)
      (error "Module `%s' already exists" name))
  (let* ((module (lookup-new-module name t))
	 (alist (memq (rassq (lookup-current-module) (lookup-module-alist))
		      lookup-module-alist)))
    (if alist
	(setcdr alist (cons (cons name module) (cdr alist)))
      (setq lookup-module-alist (nconc lookup-module-alist
				       (list (cons name module)))))
    (lookup module)
    (princ name)))

(defun lookup-nth-module (n &optional module)
  (let* ((alist (lookup-module-alist))
	 (len (length alist))
	 (pos (if module (- len (length (memq (rassq module alist) alist))) 0)))
    (setq pos (% (+ pos n) len))
    (if (< pos 0) (setq pos (+ pos len)))
    (cdr (nth pos alist))))

(defun lookup-forward-module (&optional arg)
  (interactive "p")
  (let ((module (lookup-nth-module arg (or (lookup-current-module)
					   (lookup-default-module)))))
    (if (eq major-mode 'lookup-select-mode)
	(lookup-select-session module)
      (let ((query (lookup-session-query (lookup-current-session))))
	(if (not (eq (lookup-query-method query) 'reference))
	    (lookup-search-session module query)
	  (error "Error"))))
    (princ (lookup-module-name module))))

(defun lookup-backward-module (&optional arg)
  (interactive "p")
  (lookup-forward-module (- arg)))

(defun lookup-next-history (&optional arg)
  (interactive "p")
  (let ((length (lookup-history-length lookup-search-history))
	(position (lookup-history-position lookup-search-history)))
    (setq arg (or arg 1))
    (cond
     ((= length 0) (error "No session in the history"))
     ((and (= position 1) (< arg 0)) (error "This is the first session"))
     ((and (= position length) (> arg 0)) (error "This is the last session")))
    (setq position (+ position arg))
    (setq position (if (< position 1) 1
		     (if (> position length) length
		       position)))
    (lookup-history-set-position lookup-search-history position)
    (lookup-session-display (lookup-history-ref lookup-search-history))
    (princ position)))

(defun lookup-previous-history (&optional arg)
  (interactive "p")
  (lookup-next-history (- (or arg 1))))

(defun lookup-select-dictionary ()
  (interactive)
  (lookup-select-session (lookup-current-module)))

(defun lookup-find-pattern (pattern)
  (interactive
   (let ((query (lookup-session-query (lookup-current-session))))
     (list (lookup-read-string "Look up" nil 'lookup-input-history
			       (if query (lookup-query-string query))))))
  (lookup-search-pattern (lookup-current-module) pattern))

(defun lookup-list-bookmarks ()
  (interactive)
  (let ((entries (lookup-module-bookmarks (lookup-current-module))))
    (if entries
	(let ((query (lookup-new-query 'reference "Bookmarks")))
	  (lookup-display-entries (lookup-current-module) query entries))
      (error "No bookmark in this module"))))

(defun lookup-list-history ()
  (interactive)
  (lookup-history-display (lookup-current-module)))

(defun lookup-open-window ()
  (interactive)
  (if (window-live-p lookup-start-window)
      (delete-other-windows)
    (lookup-suspend))
  (lookup))

(defun lookup-return ()
  (interactive)
  (if (window-live-p lookup-start-window)
      (select-window lookup-start-window)
    (lookup-suspend)
    (let ((lookup-open-function 'lookup-other-window))
      (lookup))
    (select-window lookup-start-window)))

(defun lookup-suspend ()
  "Close all Lookup windows temporary.
The last states of windows will be recovered if the varialbe
`lookup-save-configuration' is non-nil.  Type `\\[lookup]'
to back to Lookup."
  (interactive)
  (if (lookup-exclusive-frame-p)
      (delete-frame)
    (lookup-foreach 'lookup-hide-buffer lookup-buffer-list)
    (when (and lookup-save-configuration lookup-window-configuration)
      (set-window-configuration lookup-window-configuration)
      (setq lookup-window-configuration nil))))

(defun lookup-exit ()
  "Exit Lookup and related processes."
  (interactive)
  (if (not lookup-last-session)
      (if (interactive-p) (error "Lookup have not started"))
    (when (or (not (interactive-p))
	      (y-or-n-p "Are you sure to exit Lookup? "))
      (message "Exitting Lookup...")
      (if lookup-cache-file (lookup-dump-cache lookup-cache-file))
      (lookup-suspend)
      (lookup-foreach 'kill-buffer lookup-buffer-list)
      (lookup-foreach (lambda (pair) (lookup-dictionary-kill (cdr pair)))
		      lookup-dictionary-alist)
      (lookup-foreach (lambda (pair) (lookup-agent-kill (cdr pair)))
		      lookup-agent-alist)
      (setq lookup-buffer-list nil)
      (setq lookup-agent-alist nil)
      (setq lookup-module-alist nil)
      (setq lookup-dictionary-alist nil)
      (setq lookup-entry-table nil)
      (setq lookup-default-module nil)
      (setq lookup-current-session nil)
      (setq lookup-last-session nil)
      (message "Exitting Lookup...done"))))

(defun lookup-leave ()
  "Leave the current buffer.
If this is the first session, this is the same with \\[lookup-suspend].
Otherwise, this is the same with \\[lookup-previous-history]."
  (interactive)
  (if (> (lookup-history-position lookup-search-history) 1)
      (lookup-previous-history)
    (lookup-suspend)))

(defun lookup-restart ()
  "Exit Lookup, initialize it again, and restart."
  (interactive)
  (when (or (not (interactive-p))
	    (yes-or-no-p "Are you sure to restart Lookup? "))
    (lookup-exit)
    (lookup-initialize)
    (lookup)))

(defun lookup-help ()
  (interactive)
  (let ((help lookup-mode-help))
    (with-current-buffer (lookup-open-buffer (lookup-help-buffer))
      (help-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert help))
      (goto-char (point-min))
      (if (window-live-p lookup-start-window)
	  (set-window-buffer lookup-start-window (current-buffer))
	(display-buffer (current-buffer))))))


;;;;;;;;;;;;;;;;;;;;
;; Setup Functions
;;;;;;;;;;;;;;;;;;;;

(put 'lookup-set-agent-options 'lisp-indent-function 1)
;;;###autoload
(defun lookup-set-agent-options (id &rest options)
  (let ((plist (lookup-assoc-ref 'lookup-agent-option-alist id)))
    (while options
      (setq plist (plist-put plist (car options) (cadr options)))
      (setq options (cddr options)))
    (lookup-assoc-set 'lookup-agent-option-alist id plist)))

(put 'lookup-set-dictionary-options 'lisp-indent-function 1)
;;;###autoload
(defun lookup-set-dictionary-options (id &rest options)
  (let ((plist (lookup-assoc-ref 'lookup-dictionary-option-alist id)))
    (while options
      (setq plist (plist-put plist (car options) (cadr options)))
      (setq options (cddr options)))
    (lookup-assoc-set 'lookup-dictionary-option-alist id plist)))

;;;###autoload
(defun lookup-use-complement (id file)
  (lookup-assoc-set 'lookup-complement-alist id file))


;;;;;;;;;;;;;;;;;;;;
;; General Functions
;;;;;;;;;;;;;;;;;;;;

;;;
;;; Search functions
;;;

(defvar lookup-input-history nil
  "History of inputed search patterns.")

(defvar lookup-input-module-history nil
  "History of inputed module names.")

(defvar lookup-input-dictionary-history nil
  "History of inputed dictionary-IDs.")

(defun lookup-input-pattern ()
  (let ((default (lookup-current-word)))
    (if (string-equal default "") (setq default nil))
    (lookup-read-string "Look up" nil 'lookup-input-history default t)))

(defun lookup-input-module ()
  (lookup-get-module
   (completing-read "Search module: " (lookup-module-alist) nil t nil
		    lookup-input-module-history)))

(defun lookup-input-dictionary ()
  (lookup-get-dictionary
   (completing-read "Dictionary: " (lookup-dictionary-alist) nil t nil
		    lookup-input-dictionary-history)))

(defun lookup-search-pattern (module pattern)
  (if (> (length pattern) 80)
      (error "Too long query"))
  (if (string-match "\n" pattern)
      (error "Query should be one line"))
  (let ((query (if lookup-search-method
		   (lookup-new-query lookup-search-method pattern)
		 (lookup-parse-pattern pattern))))
    (if (string= (lookup-query-string query) "")
	(error "Invalid query pattern"))
    (when (or (not (eq (lookup-query-method query) 'text))
	      (eq lookup-search-method 'text)
	      (y-or-n-p "Are you sure to search text? "))
      (lookup-search-session module query))))

(defun lookup-stemming-search (dictionary query)
  (let* ((string (downcase (lookup-query-string query)))
	 (query (lookup-new-query 'exact string))
	 entries dict heading last-entry)
    (if (setq entries (reverse (lookup-dictionary-search dictionary query)))
	(setq last-entry (lookup-entry-substance (car entries))))
    (lookup-foreach
     (lambda (string)
       (when (> (length string) 3)
	 (lookup-query-set-string query string)
	 (lookup-foreach
	  (lambda (entry)
	    ;; Sometimes stemming generates duplicate entries with different
	    ;; headings.  Let's remove them.
	    (unless (eq (lookup-entry-substance entry) last-entry)
	      (setq entries (cons (lookup-new-slink entry) entries))
	      (setq last-entry (lookup-entry-substance entry))))
	  (lookup-dictionary-search dictionary query))))
     (cdr (nreverse (stem-english string))))
    (nreverse entries)))


;;;;;;;;;;;;;;;;;;;;
;; Session Interface
;;;;;;;;;;;;;;;;;;;;

(put 'lookup-start-session 'lisp-indent-function 2)
(defmacro lookup-start-session (type module &rest body)
  (` (unwind-protect
	 (let ((lookup-current-session (lookup-make-session (, type) (, module))))
	   (,@ body)
	   (unless (eq lookup-last-session lookup-current-session)
	     (lookup-open-session lookup-current-session)))
       (setq lookup-current-session lookup-last-session))))

(defun lookup-open-session (session)
  (let ((position (lookup-history-position lookup-search-history)))
    (if (and (> position 0)
	     (lookup-compare-session session (lookup-history-ref lookup-search-history)))
	(lookup-history-set-position lookup-search-history (1- position)))
    (lookup-session-display session)
    (lookup-history-push lookup-search-history session)))

(defun lookup-default-module ()
  (let ((name (or (lookup-assq-get lookup-mode-module-alist major-mode)
		  (lookup-assq-get lookup-mode-module-alist t))))
    (if name
	(or (lookup-get-module name)
	    (error "No such module: %s" name))
      (or lookup-default-module
	  (setq lookup-default-module (cdar (lookup-module-alist)))))))

(defun lookup-current-session ()
  (or lookup-current-session
      (if (window-live-p lookup-main-window)
	  (with-current-buffer (window-buffer lookup-main-window)
	    lookup-current-session))))

(defun lookup-current-module ()
  (let ((session (lookup-current-session)))
    (if session
	(lookup-session-module session))))


;;;;;;;;;;;;;;;;;;;;
;; Buffer Management
;;;;;;;;;;;;;;;;;;;;

;;;
;;; Lookup buffers
;;;

(defconst lookup-summary-buffer " *Summary*")
(defconst lookup-content-buffer " *Content*")
(defconst lookup-history-buffer " *Search History*")
(defconst lookup-select-buffer " *Dictionary List*")
(defconst lookup-record-buffer " *Search Records*")
(defconst lookup-help-buffer "*Lookup Help*")

(defsubst lookup-summary-buffer () lookup-summary-buffer)
(defsubst lookup-content-buffer () lookup-content-buffer)
(defsubst lookup-history-buffer () lookup-history-buffer)
(defsubst lookup-select-buffer () lookup-select-buffer)
(defsubst lookup-record-buffer () lookup-record-buffer)
(defsubst lookup-help-buffer () lookup-help-buffer)

(defsubst lookup-temp-buffer ()
  (generate-new-buffer " *Lookup temp buffer*"))

(defsubst lookup-open-process-buffer (name)
  (if lookup-debug-mode (generate-new-buffer name)))

;;;
;;; Buffer utils
;;;

(defvar lookup-start-window nil)
(defvar lookup-main-window nil)
(defvar lookup-sub-window nil)

(defun lookup-open-buffer (name)
  (let ((buffer (get-buffer-create name)))
    (if (not (memq buffer lookup-buffer-list))
	(setq lookup-buffer-list (cons buffer lookup-buffer-list)))
    buffer))

(defun lookup-hide-buffer (buffer)
  (let ((window (get-buffer-window buffer)))
    (when window
      (if (eq window lookup-main-window) (setq lookup-main-window nil))
      (if (eq window lookup-sub-window) (setq lookup-sub-window nil))
      (if (> (count-windows) 1)
	  (delete-window window)
	(switch-to-buffer (other-buffer)))))
  (if (window-live-p lookup-main-window)
      (select-window lookup-main-window))
  (bury-buffer buffer))

(defun lookup-pop-to-buffer (buffer)
  (if (and (window-live-p lookup-main-window)
	   (if (fboundp 'frame-visible-p)
	       (frame-visible-p (window-frame lookup-main-window))))
      (progn
	(set-window-buffer (select-window lookup-main-window) buffer)
	(raise-frame (window-frame lookup-main-window)))
    (setq lookup-start-window (selected-window))
    (if lookup-save-configuration
	(setq lookup-window-configuration (current-window-configuration)))
    (funcall lookup-open-function buffer)
    (setq lookup-main-window (get-buffer-window buffer t)))
  (if (window-live-p lookup-sub-window)
      (delete-window lookup-sub-window))
  buffer)

(defun lookup-display-buffer (buffer)
  (if (window-live-p lookup-sub-window)
      (set-window-buffer lookup-sub-window buffer)
    (when (or (eq (count-windows) 1) (eq (next-window) lookup-start-window))
      (let ((height (if (integerp lookup-window-height)
			lookup-window-height
		      (round (* (window-height) lookup-window-height)))))
	(split-window lookup-main-window (1+ height))))
    (let ((pop-up-frames nil))
      (display-buffer buffer))
    (setq lookup-sub-window (get-buffer-window buffer)))
  buffer)

(defun lookup-full-screen (buffer)
  (delete-other-windows)
  (switch-to-buffer buffer))

(defun lookup-other-window (buffer)
  (let ((pop-up-windows t)
	(pop-up-frames nil))
    (pop-to-buffer buffer)))

(defun lookup-other-frame (buffer)
  (let ((pop-up-frames t)
	(default-frame-alist (cons '(name . "Lookup") lookup-frame-alist)))
    (pop-to-buffer buffer)))

(defun lookup-exclusive-frame-p ()
  (string= (frame-parameter (selected-frame) 'name) "Lookup"))


;;;;;;;;;;;;;;;;;;;;
;; Arrange & Adjust
;;;;;;;;;;;;;;;;;;;;

;;;
;;; Interface functions
;;;

(defun lookup-arrange-content (entry)
  (let ((funcs (lookup-dictionary-arranges (lookup-entry-dictionary entry))))
    (lookup-format-internal entry funcs "formatting")))

(defun lookup-adjust-content (entry)
  (let ((funcs '(lookup-adjust-show-gaijis
		 lookup-adjust-hide-examples
		 lookup-adjust-check-references)))
    (if (featurep 'xemacs)
	(mapcar-extents 'delete-extent)
      (let ((overlay (overlay-lists)))
	(lookup-foreach 'delete-overlay (nconc (car overlay) (cdr overlay)))))
    (lookup-format-internal entry funcs nil)
    (goto-char (point-min))))

(defun lookup-format-internal (entry functions work)
  (let ((n 1))
    (lookup-foreach (lambda (func)
		      (when func
			(if work
			    (lookup-proceeding-message
			     (concat work (make-string (setq n (1+ n)) ?.))))
			(widen)
			(goto-char (point-min))
			(funcall func entry)))
		    functions)))

;;;
;;; Arrange/Adjust functions
;;;

;; replace

(defun lookup-arrange-replaces (entry)
  (let* ((dictionary (lookup-entry-dictionary entry))
	 (alist (lookup-dictionary-option dictionary ':replace-alist))
	 regexp string)
    (while alist
      (goto-char (point-min))
      (setq regexp (caar alist) string (cdar alist) alist (cdr alist))
      (while (re-search-forward regexp nil t)
	(replace-match string)))))

;; structure

(defun lookup-arrange-structure (entry)
  (lookup-make-region-heading (point) (progn (end-of-line) (point)) 1))

(defun lookup-adjust-hide-examples (entry)
  (unless lookup-enable-example
    (lookup-map-over-property
     (point-min) (point-max) 'lookup-heading
     (lambda (start end level)
       (when (> level 5)
	 (if (eq (char-after (1- start)) ?\n)
	     (setq start (1- start)))
	 (let ((overlay (make-overlay start (1- end))))
	   (overlay-put overlay 'invisible t)
	   (overlay-put overlay 'evaporate t)
	   (overlay-put overlay 'before-string "...")))))))

;; reference

(defun lookup-arrange-references (entry)
  (let* ((dictionary (lookup-entry-dictionary entry))
	 (pattern (lookup-dictionary-reference-pattern dictionary)))
    (when pattern
      (when (functionp pattern)
	(setq pattern (funcall pattern entry)))
      (let ((case-fold-search nil)
	    (regexp (car pattern)) (link-item (nth 1 pattern))
	    (heading-item (nth 2 pattern)) (code-item (nth 3 pattern))
	    start link heading code entry)
	(while (re-search-forward regexp nil t)
	  (setq start (match-beginning 0))
	  (setq link (if (integerp link-item)
			 (match-string link-item)
		       (save-match-data (eval link-item))))
	  (setq heading (if (integerp heading-item)
			    (match-string heading-item)
			  (save-match-data (eval heading-item))))
	  (setq code (cond ((not code-item) heading)
			   ((integerp code-item) (match-string code-item))
			   (t code-item)))
	  (replace-match link t t)
	  (if (stringp code)
	      (setq entry (lookup-new-entry 'regular dictionary code heading))
	    (setq entry (lookup-new-entry 'dynamic dictionary heading))
	    (lookup-entry-put-property entry ':dynamic code))
	  (lookup-set-link start (point) entry))))))

(defun lookup-adjust-check-references (entry)
  (lookup-map-over-property
   (point-min) (point-max) 'lookup-reference
   (lambda (start end entry)
     (if (lookup-entry-refered-p entry)
	 (put-text-property start end 'face 'lookup-refered-face)
       (put-text-property start end 'face 'lookup-reference-face)))))

(defun lookup-dynamic-search (entry)
  (let ((query (lookup-new-query 'exact (lookup-entry-code entry))))
    (lookup-dictionary-search (lookup-entry-dictionary entry) query)))

;; gaiji

(defun lookup-arrange-gaijis (entry)
  (let ((case-fold-search nil)
	(dictionary (lookup-entry-dictionary entry))
	regexp start end gaiji)
    (when (setq regexp (lookup-dictionary-gaiji-regexp dictionary))
      (while (re-search-forward regexp nil t)
	(setq start (match-beginning 0) end (match-end 0))
	(setq gaiji (lookup-dictionary-gaiji dictionary (match-string 1)))
	(when gaiji
	  (delete-region start end)
	  (lookup-gaiji-insert gaiji))))))

(defun lookup-adjust-show-gaijis (entry)
  (when lookup-enable-gaiji
    (lookup-map-over-property
     (point-min) (point-max) 'lookup-gaiji
     (lambda (start end gaiji)
       (lookup-gaiji-glyph-paste start end (lookup-gaiji-glyph gaiji))))))

;; fill

(defun lookup-arrange-nofill (entry))

(defun lookup-arrange-fill-lines (entry)
  (text-mode)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column))))
	start)
    (while (not (eobp))
      (setq start (point))
      (end-of-line)
      (if (> (current-column) fill-column) (fill-region start (point)))
      (forward-line))))

(defun lookup-arrange-fill-paragraphs (entry)
  (text-mode)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column)))))
    (while (not (eobp))
      (fill-paragraph nil)
      (forward-paragraph))))

;;;
;;; Internal functions
;;;

(defun lookup-heading-face (level)
  (or (nth (1- level) '(lookup-heading-1-face
			lookup-heading-2-face lookup-heading-3-face
			lookup-heading-4-face lookup-heading-5-face))
      'lookup-heading-low-face))

(defun lookup-make-region-heading (start end level)
  (add-text-properties start end (list 'face (lookup-heading-face level)
				       'lookup-heading level)))

(defun lookup-set-link (start end reference)
  (add-text-properties start end (list 'mouse-face 'highlight
				       'lookup-reference reference)))

(defun lookup-get-link (position)
  (get-text-property position 'lookup-reference))

(defun lookup-goto-next-link ()
  (let ((p (point)))
    (and (setq p (next-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (next-single-property-change p 'lookup-reference)))
	 (goto-char p))))

(defun lookup-goto-previous-link ()
  (let ((p (point)))
    (and (setq p (previous-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (previous-single-property-change p 'lookup-reference)))
	 (goto-char p))))


;;;;;;;;;;;;;;;;;;;;
;; Internal Functions
;;;;;;;;;;;;;;;;;;;;

(put 'lookup-error 'error-conditions '(error))
(put 'lookup-error 'error-message "Lookup error")

(put 'lookup-no-valid-dictionary 'error-conditions '(lookup-error error))
(put 'lookup-no-valid-dictionary 'error-message "No valid dictionary for the method")

(put 'lookup-no-entry-error 'error-conditions '(lookup-error error))
(put 'lookup-no-entry-error 'error-message "No entry for the query")

(defconst lookup-obarray (make-vector 1511 nil))

(defsubst lookup-intern-string (string)
  (symbol-name (intern string lookup-obarray)))

(defun lookup-proceeding-message (work)
  (when lookup-proceeding-message
    (let ((message (concat lookup-proceeding-message "...")))
      (cond
       ((not work) (message "%s" message))
       ((eq work t) (message "%s" (concat message "done")))
       (t (message "%s" (concat message " (" work ")")))))))


;;;
;;; Provide Lookup
;;;

(provide 'lookup)

(unless lookup-byte-compiling
  (add-hook 'kill-emacs-hook 'lookup-exit)
  (lookup-initialize))

;;; lookup.el ends here
