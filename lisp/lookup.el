;;; lookup.el --- Search interface to electronic dictionaries
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

(require 'cl)
(require 'lookup-types)
(require 'lookup-utils)
(require 'lookup-vars)

(unless lookup-byte-compiling
  (load "lookup-autoloads"))

(defconst lookup-version "1.99.7"
  "The version numbers of Lookup.")


;;;
;;; Top-level
;;;

;;;###autoload
(defun lookup-version (arg)
  "Display the version string of Lookup.
With prefix argument, insert string at point."
  (interactive "P")
  (let ((version (concat "Lookup " lookup-version)))
    (if arg (insert version) (message version))))

;;;###autoload
(defun lookup ()
  "Start Lookup and display the list of your dictionaries.
If you have already started lookup, display the last status of buffers."
  (interactive)
  (let ((session (and lookup-search-history
                      (lookup-history-ref  lookup-search-history))))
    (if session
	(lookup-session-display session)
      (lookup-initialize)
      (lookup-select-dictionaries (lookup-default-module)))))

(defun lookup-kill ()
  "Force Lookup to be quiet when you exit Emacs.
This can be used when you cannot finish Emacs because of an error of Lookup."
  (interactive)
  (lookup-clear)
  (message "OK, you can exit Emacs"))

(defun lookup-debug ()
  "Toggle Lookup debug mode."
  (interactive)
  (setq lookup-enable-debug (not lookup-enable-debug))
  (setq debug-on-error lookup-enable-debug)
  (message (if lookup-enable-debug
	       "Lookup debug enabled"
	     "Lookup debug disabled")))


;;;;;;;;;;;;;;;;;;;;
;; Internal Functions
;;;;;;;;;;;;;;;;;;;;

(defvar lookup-message nil)

(put 'lookup-with-message 'lisp-indent-function 1)
(defmacro lookup-with-message (msg &rest body)
  `(let ((lookup-message ,msg))
     (message "%s..." lookup-message)
     (prog1 (progn ,@body)
       (message "%s...done" lookup-message))))

(defun lookup-message (msg)
  (message "%s... (%s)" lookup-message msg))



;;;
;;; Global commands
;;;

(defvar lookup-global-map nil)

(unless lookup-global-map
  (setq lookup-global-map (make-sparse-keymap))
  (define-key lookup-global-map "\C-\M-n" 'lookup-next-history)
  (define-key lookup-global-map "\C-\M-p" 'lookup-previous-history)
  (define-key lookup-global-map "\C-\M-f" 'lookup-forward-module)
  (define-key lookup-global-map "\C-\M-b" 'lookup-backward-module)
  (define-key lookup-global-map "B" 'lookup-list-bookmarks)
  (define-key lookup-global-map "H" 'lookup-list-history)
  (define-key lookup-global-map "f" 'lookup-find-pattern)
  (define-key lookup-global-map "o" 'lookup-open-window)
  (define-key lookup-global-map "r" 'lookup-return)
  (define-key lookup-global-map "q" 'lookup-suspend)
  (define-key lookup-global-map "Q" 'lookup-exit)
  (define-key lookup-global-map "R" 'lookup-restart)
  (define-key lookup-global-map "?" 'lookup-help))

(defun lookup-nth-module (n &optional module)
  (let* ((len (length lookup-module-list))
	 (pos (if module (position module lookup-module-list) 0)))
    (setq pos (% (+ pos n) len))
    (if (< pos 0) (setq pos (+ pos len)))
    (nth pos lookup-module-list)))

(defun lookup-forward-module (arg)
  "Forward current module by ARG.
New session begins if there is a session.  If called from 
`lookup-select-mode' or `lookup-modules-mode', then default
module will be changed."
  (interactive "p")
  (let ((session (lookup-current-session)) 
        module query)
    (if (or (eq major-mode 'lookup-select-mode)
            (eq major-mode 'lookup-modules-mode)
            (null session))
        (progn
          (dotimes (x (mod arg (length lookup-module-list)))
            (setq lookup-module-list (nconc (cdr lookup-module-list)
                                            (list (car lookup-module-list)))))
          (setq module (car lookup-module-list))
          (cond ((eq major-mode 'lookup-select-mode)
                 (setf (lookup-session-module session) module)
                 (lookup-select-dictionaries module))
                ((eq major-mode 'lookup-modules-mode)
                 (setf (lookup-session-module session) module)
                 (lookup-list-modules))
                (t (message "Current module in this buffer is %s" 
                            (lookup-current-module)))))
      (setq module (lookup-nth-module arg (lookup-current-module)))
      (setq query (lookup-session-query session))
      (if (not (eq (lookup-query-method query) 'reference))
          (lookup-search-query module query)
        (error
         "Current session handles `reference'.  Please exit session first")))
    (princ (lookup-module-name module))))

(defun lookup-backward-module (arg)
  (interactive "p")
  (lookup-forward-module (- arg)))

(defun lookup-next-history (&optional arg)
  (interactive "p")
  (let ((session (lookup-history-move lookup-search-history (or arg 1))))
    (lookup-session-display session))
  (princ (lookup-history-position lookup-search-history)))

(defun lookup-previous-history (&optional arg)
  (interactive "p")
  (lookup-next-history (- (or arg 1))))

(defun lookup-find-pattern (pattern)
  (interactive
   (let* ((session (lookup-current-session))
	  (default (if session
		       (lookup-query-string (lookup-session-query session)))))
     (list (lookup-read-string "Look up" nil 'lookup-input-history default))))
  (lookup-search-pattern (lookup-current-module) pattern))

(defun lookup-list-bookmarks ()
  (interactive)
  (let ((entries (lookup-module-bookmarks (lookup-current-module))))
    (if entries
	(let ((query (lookup-new-query 'reference "Bookmarks")))
	  (lookup-display-entries (lookup-current-module) query entries))
      (error "This module has no bookmark"))))

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
Type `\\[lookup]' to back to Lookup."
  (interactive)
  (if (lookup-exclusive-frame-p)
      (delete-frame)
    (mapc 'lookup-hide-buffer lookup-buffer-list)
    (when lookup-window-configuration
      (set-window-configuration lookup-window-configuration)
      (setq lookup-window-configuration nil))))

(defun lookup-exit ()
  "Exit Lookup and related processes."
  (interactive)
  (if (not lookup-last-session)
      (if (interactive-p) (error "Lookup is not started"))
    (when (or (not (interactive-p))
	      (y-or-n-p "Are you sure to exit Lookup? "))
      (lookup-with-message "Exitting Lookup"
	(if lookup-cache-file (lookup-dump-cache lookup-cache-file))
	(lookup-suspend)
	(mapc 'kill-buffer lookup-buffer-list)
	(mapc 'lookup-agent-clear lookup-agent-list)
	(setq lookup-buffer-list nil)
	(setq lookup-agent-list nil)
	(setq lookup-module-list nil)
	(setq lookup-dictionary-list nil)
	(setq lookup-entry-table nil)
	(setq lookup-current-session nil)
        (setq lookup-search-history nil)
	(setq lookup-last-session nil)))))

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
    (setq lookup-property-table nil)
    (lookup-exit)
    (when (and (interactive-p)
               (file-exists-p lookup-cache-file)
               (y-or-n-p "Delete cache (Warning: module info will be lost.) ? "))
      (delete-file lookup-cache-file)
      (setq lookup-search-modules nil)
      (setq lookup-agent-attributes nil)
      (setq lookup-dictionary-attributes nil))
    (lookup)))

(defun lookup-help ()
  (interactive)
  (let ((lookup-help lookup-help-message))
  (with-current-buffer (lookup-get-buffer "*Lookup Help*")
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lookup-help))
    (goto-char (point-min))
    (if (window-live-p lookup-start-window)
	(set-window-buffer lookup-start-window (current-buffer))
      (display-buffer (current-buffer))))))

(defun lookup-filter-query (query filters)
  "Apply FILTERS functions to QUERY to generate filtered queries."
  (let ((queries (list query)) result)
    (mapcar 
     (lambda (filter)
       (setq queries 
             (apply 'append 
                    (mapcar (lambda (query)
                              (setq result (apply filter (list query)))
                              (message "debug: result=%s" result)
                              (if (listp result) result (list result)))
                            queries)))
       (message "debug: queries=%s" queries))
     filters)
    queries))

(defun lookup-filter-string (string filters)
  (let ((query (lookup-new-query 'default string)))
    (lookup-query-string 
     (car (lookup-filter-query query filters)))))


;;;
;;; Search commands
;;;

(defun lookup-pattern-input ()
  (let* ((module (lookup-input-module-interactively))
	 (pattern (lookup-input-pattern module)))
    (list pattern module)))

;;;###autoload
(defun lookup-pattern (pattern &optional module)
  "Search for the PATTERN with MODULE."
  (interactive (lookup-pattern-input))
  (lookup-search-pattern module pattern))

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
  (let ((module (lookup-input-module-interactively))
	(word (lookup-current-word)))
    (list word module)))

;;;###autoload
(defun lookup-word (word &optional module)
  "Search for the word near the cursor."
  (interactive (lookup-word-input))
  (if lookup-edit-input 
      (lookup-search-pattern module (lookup-input-pattern module word))
    (lookup-search-pattern
     module 
     (lookup-filter-string string lookup-query-filters)
     lookup-default-method)))

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
  (let ((module (lookup-input-module-interactively))
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


;;;
;;; Search functions
;;;

(defvar lookup-input-history nil
  "History of inputed search patterns.")

(defvar lookup-input-module-history nil
  "History of inputed module names.")

(defvar lookup-input-dictionary-history nil
  "History of inputed dictionary-IDs.")

(defun lookup-input-pattern (module &optional default)
  (or default (setq default (lookup-current-word)))
  (setq default (lookup-filter-string default lookup-query-filters))
  (if (string-equal default "") (setq default nil))
  (lookup-read-string
   (concat (if module (concat "[" (lookup-module-name module) "] "))
           "Look up")
   default 'lookup-input-history default t))

(defun lookup-input-module-interactively ()
  (if current-prefix-arg (lookup-input-module) (lookup-current-module)))

(defun lookup-input-module ()
  (let ((table (mapcar (lambda (module) (lookup-module-name module))
                       lookup-module-list)))
    (lookup-get-module
     (completing-read "Search module: " table nil t nil
                      'lookup-input-module-history)))
  (lookup-default-module))

(defun lookup-input-dictionary (&optional module)
  "Let the user input dictionary.
If MODULE is specified, then dictionaries NOT in a module are shown as
candidates."
  (let ((table (mapcar (lambda (dict) (lookup-dictionary-id dict))
                       (if module
                           (set-difference lookup-dictionary-list
                                           (lookup-module-dictionaries module))
                         lookup-dictionary-list))))
    (if (null table)
        (error "Module has all dictionaries"))
    (lookup-get-dictionary
     (completing-read "Dictionary: " table nil t nil
                      'lookup-input-dictionary-history))))

(defun lookup-search-pattern (module pattern &optional method)
  (cond ((> (length pattern) 80) (error "Too long query"))
	((string-match "\n" pattern) (error "Query should be one line")))
  (let ((query (if method (lookup-new-query method pattern)
		 (lookup-parse-pattern pattern))))
    (when (or (not (eq (lookup-query-method query) 'text))
	      (y-or-n-p "Are you sure to search text? "))
      (lookup-search-query module query))))

(defun lookup-search-query (module query)
;  (if (and lookup-last-session
;	    (let ((last (lookup-session-query lookup-last-session)))
;	      (and (eq (lookup-query-method query)
;		       (lookup-query-method last))
;		   (string= (lookup-query-string query)
;			    (lookup-query-string last)))))
;      (lookup-session-display  (lookup-history-ref lookup-search-history))
    (lookup-with-message (format "Looking up `%s'" (lookup-query-pattern query))
      (let ((lookup-dynamic-display t)
	    found valid)
        ; ** multiple module search is temporary off. **
	;(do ((mod module next-module)
	;     (next-module nil (lookup-nth-module 1 mod)))
	;    ((or found (eq next-module module)))
	(let ((mod module))
	  (dolist (dict (or lookup-search-dictionaries
			    (lookup-module-dictionaries mod)))
	    (when (and
		   ;; Check dictionary priority
		   (or lookup-search-dictionaries
		       (let ((p (lookup-module-dictionary-priority mod dict)))
			 (cond ((eq p t) t)
			       ((eq p 'secondary) (not found))
			       ((eq p 'supplement) found))))
		   ;; Check search method
		   (let ((method (lookup-query-method query)))
		     (or (eq method 'default)
			 (memq method (lookup-dictionary-methods dict)))))
	      (setq valid t)
	      (lookup-message (concat (lookup-dictionary-title dict) "..."))
	      (let ((entries (lookup-dictionary-search dict query)))
		(when entries
		  (if found
		      (lookup-summary-append entries)
		    (setq found t)
		    (let ((session (lookup-new-session mod query entries)))
		      (lookup-session-open session))))))))
	(cond
	 ((not valid) (error "[%s] No valid dictionary for method `%s'"
                             (lookup-module-name module)
			     (lookup-query-method query)))
	 ((not found) (error "[%s] No entry for query: %s"
                             (lookup-module-name module)
			     (lookup-query-pattern query)))))))

(defun lookup-display-entries (module query entries)
  (lookup-session-open (lookup-new-session module query entries)))


;;;
;;; Arrange & Adjust
;;;

(defun lookup-arrange-content (entry)
  (let ((funcs (lookup-dictionary-content-arranges
                (lookup-entry-dictionary entry))))
    (save-excursion
      (lookup-format-internal entry funcs "formatting"))))

(defun lookup-adjust-content (entry)
  (let ((funcs '(lookup-adjust-show-gaijis
		 lookup-adjust-hide-examples
		 lookup-adjust-check-references)))
    (if (featurep 'xemacs)
	(mapcar-extents 'delete-extent)
      (let ((overlay (overlay-lists)))
	(mapc 'delete-overlay (car overlay))
	(mapc 'delete-overlay (cdr overlay))))
    (lookup-format-internal entry funcs nil)
    (goto-char (point-min))))

(defun lookup-format-internal (entry functions msg)
  (let ((n 1))
    (dolist (func functions)
      (when func
	(if msg
	    (lookup-message (concat msg (make-string (setq n (1+ n)) ?.))))
	(widen)
	(goto-char (point-min))
	(funcall func entry)))))

;; replace

(defun lookup-arrange-replaces (entry)
  (dolist (pair (lookup-dictionary-option (lookup-entry-dictionary entry)
					  :replace-alist))
    (goto-char (point-min))
    (while (re-search-forward (car pair) nil t)
      (replace-match (cdr pair)))))

;; structure

(defun lookup-arrange-structure (entry)
  (lookup-make-region-heading (point) (line-end-position) 1))

(defun lookup-adjust-hide-examples (entry)
  (unless lookup-enable-example
    (lookup-map-over-property
     (point-min) (point-max) 'face
     (lambda (start end face)
       (when (eq face 'lookup-comment-face)
	 (if (eq (char-after (1- start)) ?\n)
	     (setq start (1- start)))
	 (let ((overlay (make-overlay start (1- end))))
	   (overlay-put overlay 'invisible t)
	   (overlay-put overlay 'evaporate t)
	   (overlay-put overlay 'before-string "...")))))))

;; reference

(defun lookup-arrange-references (entry)
  "Arrange reference of ENTRY.
:reference-pattern should be (regexp link-item heading code).
link-item, heading, or code may be integer or function."
  (let* ((dict (lookup-entry-dictionary entry))
	 (pattern (lookup-dictionary-reference-pattern dict)))
    (when pattern
      (when (functionp pattern)
	(setq pattern (funcall pattern entry)))
      (let ((case-fold-search nil)
	    (regexp (car pattern)) (link-item (nth 1 pattern))
	    (heading-item (nth 2 pattern)) (code-item (nth 3 pattern)))
	(while (re-search-forward regexp nil t)
	  (let* ((start (match-beginning 0))
		 (link (if (integerp link-item)
			   (match-string link-item)
			 (save-match-data (eval link-item))))
		 (heading (if (integerp heading-item)
			      (match-string heading-item)
			    (save-match-data (eval heading-item))))
		 (code (cond ((not code-item) heading)
			     ((integerp code-item) (match-string code-item))
			     (t code-item))))
            (if (= 0 (length heading)) (setq heading "[No Title]"))
	    (replace-match link t t)
	    (if (stringp code)
		(setq entry (lookup-new-entry 'regular dict code heading))
	      (setq entry (lookup-new-entry 'dynamic dict heading))
	      (lookup-put-property entry :dynamic code))
	    (lookup-set-link start (point) entry)))))))

(defun lookup-adjust-check-references (entry)
  (lookup-map-over-property
   (point-min) (point-max) 'lookup-reference
   (lambda (start end entry)
     (if (lookup-entry-referred-p entry)
	 (put-text-property start end 'face 'lookup-referred-face)
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
          (goto-char start)
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
  "Fill lines except `read-only' property region."
  (text-mode)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column))))
	start end read-only-start)
    (while (not (eobp))
      (setq start (point)
            end   (point-at-eol)
            read-only-start (text-property-any start end 'read-only t))
      (if read-only-start
          (progn
            (goto-char read-only-start)
            (if (> (current-column) fill-column)
                (save-excursion
                  (fill-region start (point))))
            (goto-char (text-property-not-all read-only-start (point-max) 'read-only t))
            (if (and (> (current-column) fill-column)
                     (not (eolp))) (insert "\n")))
        (goto-char end)
        (if (> (current-column) fill-column)
            (save-excursion
              (fill-region start (point))))
        (forward-line)))))

(defun lookup-arrange-fill-paragraphs (entry)
  (text-mode)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column)))))
    (fill-individual-paragraphs (point-min) (point-max))))

;; utils

(defun lookup-heading-face (level)
  (or (nth (1- level) '(lookup-heading-1-face
			lookup-heading-2-face lookup-heading-3-face
			lookup-heading-4-face lookup-heading-5-face))
      'lookup-comment-face))

(defun lookup-make-region-heading (start end level)
  (add-text-properties start end (list 'face (lookup-heading-face level)
				       'lookup-heading level)))

(defun lookup-set-link (start end reference &optional object)
  (add-text-properties start end (list 'mouse-face 'highlight
				       'lookup-reference reference)
                       object))

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


;;;
;;; Object management
;;;

(defun lookup-current-session ()
  (or lookup-current-session
      (if (window-live-p lookup-main-window)
	  (with-current-buffer (window-buffer lookup-main-window)
	    lookup-current-session))))

(defun lookup-current-module ()
  "Return current session module.
If there is no session, default module will be returned."
  (let ((session (lookup-current-session)))
    (if session (lookup-session-module session)
      (lookup-default-module))))

(defun lookup-default-module ()
  "Default module of current buffer."
  (let ((name (or (lookup-assq-get lookup-mode-module-alist major-mode)
		  (lookup-assq-get lookup-mode-module-alist t))))
    (if name
	(or (lookup-get-module name)
	    (error "No such module: %s" name))
      (car lookup-module-list))))

(defun lookup-get-module (name &optional module-list)
  (car (member-if (lambda (module) (equal (lookup-module-name module) name))
		  (or module-list lookup-module-list))))

(defun lookup-get-agent (id)
  (car (member-if (lambda (agent) (equal (lookup-agent-id agent) id))
		  lookup-agent-list)))

(defun lookup-get-dictionary (id)
  (car (member-if (lambda (dict) (equal (lookup-dictionary-id dict) id))
		  lookup-dictionary-list)))

(defun lookup-entry-list ()
  (let (entries)
    (when lookup-entry-table
      (mapatoms (lambda (symbol)
		  (setq entries (cons (symbol-value symbol) entries)))
		lookup-entry-table))
    entries))

(defun lookup-put-entry (entry)
  (unless lookup-entry-table
    (setq lookup-entry-table (make-vector 377 0)))
  (set (intern (lookup-entry-id entry) lookup-entry-table) entry))

(defun lookup-get-entry (id)
  (when lookup-entry-table
    (let ((symbol (intern id lookup-entry-table)))
      (if (boundp symbol) (symbol-value symbol)))))

(defun lookup-get-entry-create (id)
  (or (lookup-get-entry id)
      (when (string-match "#" id)
	(let ((dict (substring id 0 (match-beginning 0)))
	      (code (substring id (match-end 0))))
	  (lookup-new-entry 'regular (lookup-get-dictionary dict) code)))))

(defun lookup-gaiji-list ()
  (let (gaijis)
    (dolist (dict lookup-dictionary-list)
      (mapatoms (lambda (code)
		  (setq code (symbol-value code))
		  (if (lookup-gaiji-p code)
		      (setq gaijis (cons code gaijis))))
		(lookup-dictionary-gaiji-table dict)))
    gaijis))


;;;
;;; Buffer management
;;;

(defconst lookup-summary-buffer " *Summary*")
(defconst lookup-content-buffer " *Content*")

(defsubst lookup-summary-buffer () lookup-summary-buffer)
(defsubst lookup-content-buffer () lookup-content-buffer)

(defsubst lookup-temp-buffer ()
  (generate-new-buffer " *Lookup temp buffer*"))

(defsubst lookup-open-process-buffer (name)
  (if lookup-enable-debug (generate-new-buffer name)))

(defvar lookup-start-window nil)
(defvar lookup-main-window nil)
(defvar lookup-sub-window nil)

(defun lookup-get-buffer (name)
  (let ((buffer (get-buffer-create name)))
    (setq lookup-buffer-list (adjoin buffer lookup-buffer-list))
    buffer))

(defun lookup-pop-to-buffer (buffer)
  (if (window-live-p lookup-main-window)
      (progn
	(set-window-buffer (select-window lookup-main-window) buffer)
	(raise-frame (window-frame lookup-main-window)))
    (setq lookup-start-window (selected-window))
    (if (> (length (window-list)) 1)
	(setq lookup-window-configuration (current-window-configuration)))
    (funcall lookup-open-function buffer)
    (setq lookup-main-window (get-buffer-window buffer t)))
  (when (window-live-p lookup-sub-window)
    (delete-window lookup-sub-window)
    (setq lookup-sub-window nil))
  buffer)

(defun lookup-display-buffer (buffer)
  (if (window-live-p lookup-sub-window)
      (set-window-buffer lookup-sub-window buffer)
    (setq lookup-sub-window
	  (if (<= (window-height lookup-main-window) lookup-window-height)
	      (next-window)
	    (let ((height (if (integerp lookup-window-height)
			      lookup-window-height
			    (round (* (window-height) lookup-window-height)))))
	      (split-window lookup-main-window (1+ height)))))
    (set-window-buffer lookup-sub-window buffer))
  buffer)

(defun lookup-hide-buffer (buffer)
  (let ((window (get-buffer-window buffer)))
    (when window
      (cond ((eq window lookup-main-window)
	     (setq lookup-main-window nil))
	    ((eq window lookup-sub-window)
	     (if (window-live-p lookup-main-window)
		 (select-window lookup-main-window))
	     (setq lookup-sub-window nil)))
      (if (> (count-windows) 1)
	  (delete-window window)
	(switch-to-buffer (other-buffer)))))
  (bury-buffer buffer))

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
  "Set dictionary ID's OPTIONS plist prior to dictionary initialization."
  (let ((plist (lookup-assoc-ref 'lookup-dictionary-option-alist id)))
    (while options
      (setq plist (plist-put plist (car options) (cadr options)))
      (setq options (cddr options)))
    (lookup-assoc-set 'lookup-dictionary-option-alist id plist)))

;;;###autoload
(defun lookup-use-support (id file)
  (lookup-assoc-set 'lookup-support-alist id file))

;;;
;;; Auto-Lookup
;;;

(defvar lookup-auto-lookup-mode nil)
(defvar lookup-auto-lookup-timer nil)
(defvar lookup-auto-lookup-word "")

(defvar lookup-auto-lookup-interval 1.00)
(defvar lookup-auto-lookup-open-function 'lookup-other-window)

(defun lookup-toggle-auto-lookup()
  (interactive)
  (if lookup-auto-lookup-mode 
      (lookup-deactivate-auto-lookup)
    (lookup-activate-auto-lookup)))

(defun lookup-activate-auto-lookup ()
  "Activate Auto Lookup."
  (interactive)
  (setq lookup-auto-lookup-mode nil)
  (unless (lookup-get-module "auto")
    (error "Please prepare `auto' module before using auto-lookup!"))
  (setq lookup-auto-lookup-mode t)
  (setq lookup-auto-lookup-timer
        (run-with-idle-timer
         lookup-auto-lookup-interval t
         'lookup-auto-lookup)))

(defun lookup-deactivate-auto-lookup ()
  "Deactivate Auto Lookup."
  (interactive)
  (setq lookup-auto-lookup-mode nil)
  (cancel-timer lookup-auto-lookup-timer))

(defun lookup-auto-lookup ()
  "Execute Auto Lookup."
  (when (and (not isearch-mode)
             (not executing-kbd-macro)
             lookup-auto-lookup-mode
             (not (window-minibuffer-p (selected-window)))
             (not (eq (current-buffer) (get-buffer (lookup-summary-buffer))))
             (not (eq (current-buffer) (get-buffer (lookup-content-buffer))))
             (not (eq (current-buffer) (get-buffer " *Dictionary List*")))
             (not (eq (current-buffer) (get-buffer " *Search History*")))
             (not (eq (current-buffer) (get-buffer " *Module List*"))))
    (save-selected-window
      (save-match-data
        (save-excursion
          (save-restriction
            (let ((lookup-edit-input nil)
                  (word (lookup-current-word))
                  (lookup-open-function lookup-auto-lookup-open-function))
              (when (not (equal lookup-auto-lookup-word word))
                (lookup-word word (lookup-get-module "auto"))
                (setq lookup-auto-lookup-word word)))))))))


;;;
;;; Initialize Lookup
;;;

(defun lookup-initialize ()
  (load lookup-init-file t)
  (when lookup-cache-file
    (require 'lookup-cache)
    (load lookup-cache-file t)) 
  (setq lookup-search-history (lookup-new-history))
  (setq lookup-agent-list
	(mapcar (lambda (spec) (apply #'lookup-new-agent spec))
		(or lookup-search-agents
		    (setq lookup-search-agents '((ndtut))))))
  (setq lookup-dictionary-list
	(apply 'append
	       (mapcar 'lookup-agent-dictionaries lookup-agent-list)))
  (setq lookup-module-list
	(mapcar (lambda (spec) (apply #'lookup-new-module spec))
		(or lookup-search-modules '(("default" t)))))
  (lookup-init-support-autoload)
  (run-hooks 'lookup-load-hook)
  (add-hook 'kill-emacs-hook 'lookup-exit))

(defun lookup-init-support-autoload ()
  (load "support-defs")
  (dolist (pair lookup-support-autoload-alist)
    (dolist (dict lookup-dictionary-list)
      (when (string-match (car pair) (lookup-dictionary-id dict))
	(lookup-assoc-set 'lookup-support-alist
			  (lookup-dictionary-id dict)
			  (cdr pair))
        ))))
	;;(return))))) ;; what if we stop reading after first match?

(defun lookup-clear ()
  (remove-hook 'kill-emacs-hook 'lookup-exit))

(provide 'lookup)

(unless lookup-byte-compiling
  (lookup-initialize))

;;; lookup.el ends here
