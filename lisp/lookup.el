;;; lookup.el --- Search interface to electronic dictionaries -*- lexical-binding: t -*-
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

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'lookup-types)
(require 'lookup-select)
(require 'lookup-content)
(require 'lookup-summary)
(require 'lookup-modules)
(require 'lookup-history)
(require 'lookup-text)
(require 'lookup-cache)

(defvar lookup-start-window nil)
(defvar lookup-main-window nil)
(defvar lookup-sub-window nil)

;(unless lookup-byte-compiling
;  (load "lookup-autoloads"))

(defconst lookup-version "1.99.96"
  "The version numbers of Lookup.")


;;;; Top-level

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
      (if (not (and lookup-agent-list lookup-dictionary-list lookup-module-list))
          (lookup-initialize))
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

(defun lookup-debug-message (format-string &rest args)
  (if lookup-enable-debug
      (apply 'message (concat "lookup-debug:" format-string) args)))


;;; Internal Functions
(defvar lookup-message nil)

(defun lookup-message (msg)
  (message "%s... (%s)" lookup-message msg))



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

;;;###autoload
(defun lookup-list-history ()
  (interactive)
  (lookup-history-display (lookup-current-module)))

;;;###autoload
(defun lookup-list-modules ()
  (interactive)
  (lookup-modules-display))

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
      (if (called-interactively-p 'any)
          (error "Lookup is not started"))
    (when (or (not (called-interactively-p 'any))
              (y-or-n-p "Are you sure to exit Lookup? "))
      (lookup-with-message "Exiting Lookup"
        (if lookup-cache-file (lookup-dump-cache lookup-cache-file))
        (lookup-suspend)
        (mapc 'kill-buffer lookup-buffer-list)
        (mapc 'lookup-agent-clear lookup-agent-list)
        (lookup-clear)))))

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
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Are you sure to restart Lookup? "))
    (lookup-exit)
    (setq lookup-property-table nil)
    (setq lookup-current-session nil)
    (setq lookup-module-list nil)
    (when (and lookup-cache-file
               (file-exists-p lookup-cache-file)
               (yes-or-no-p "Do you want to remove cache file?"))
      (delete-file lookup-cache-file))
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
  (let ((module (lookup-input-module-interactively))
        (word (lookup-current-word)))
    (list word module)))

;;;###autoload
(defun lookup-word (word &optional module)
  "Search for the word near the cursor."
  (interactive (lookup-word-input))
  (or module (setq module (lookup-default-module)))
  (if lookup-edit-input 
      (lookup-search-pattern module (lookup-input-pattern module word))
    (lookup-search-pattern
     module 
     (lookup-filter-string word lookup-query-filters)
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
(defun lookup-selection (_click)
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
  (callf or default (lookup-current-word) "")
  (setq default (lookup-filter-string default lookup-query-filters))
  (if (string-equal default "") (setq default nil))
  (lookup-read-string
   (concat (if module (concat "[" (lookup-module-name module) "] "))
           "Look up")
   default 'lookup-input-history default t))

(defun lookup-input-module-interactively ()
  ;; t is e.g. `-'. etc.
  (message "current-prefix-arg=%s" current-prefix-arg)
  (typecase  current-prefix-arg
    (null (lookup-current-module))
    (list (lookup-input-module))
    (integer (nth (1+ current-prefix-arg) lookup-module-list))
    (t (lookup-default-module))))

(defun lookup-input-module ()
  (let ((table (mapcar (lambda (module) (lookup-module-name module))
                       lookup-module-list)))
    (lookup-get-module
     (completing-read "Search module: " table nil t nil
                      'lookup-input-module-history))))

(defun lookup-input-dictionary (&optional module)
  "Let the user input dictionary.
If MODULE is specified, then dictionaries NOT in a module are shown as
candidates."
  (let ((table (mapcar (lambda (dict) (lookup-dictionary-id dict))
                       (if module
                           (cl-set-difference lookup-dictionary-list
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
  ;;(if (and lookup-last-session
  ;;          (let ((last (lookup-session-query lookup-last-session)))
  ;;            (and (eq (lookup-query-method query)
  ;;                     (lookup-query-method last))
  ;;                 (string= (lookup-query-string query)
  ;;      		    (lookup-query-string last)))))
  ;;    (lookup-session-display  (lookup-history-ref lookup-search-history))
  (lookup-with-message (format "Looking up `%s'" (lookup-query-pattern query))
    (let ((lookup-dynamic-display t)
          found valid
          (mod module))
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
                  ;; when new session is created, then module for
                  ;; select buffer will become invaild.
                  (setq lookup-select-module nil)
                  (lookup-session-open session)))))))
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
;;; Arrange
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
    (let ((overlay (overlay-lists)))
      (mapc 'delete-overlay (car overlay))
      (mapc 'delete-overlay (cdr overlay)))
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

(defun lookup-arrange-structure (_entry)
  (lookup-make-region-heading (point) (line-end-position) 1))

(defun lookup-adjust-hide-examples (_entry)
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
      (let ((case-fold-search nil))
        (destructuring-bind
            (regexp link-item heading-item code-item) pattern
          (while (re-search-forward regexp nil t)
            (let* ((start (match-beginning 0))
                   (link (if (integerp link-item)
                             (match-string link-item)
                           (save-match-data (eval link-item))))
                   (heading (if (integerp heading-item)
                                (match-string heading-item)
                              (save-match-data (eval heading-item))))
                   (code (typecase code-item
                           (null    heading)
                           (integer (match-string code-item))
                           (t       code-item))))
              (if (= 0 (length heading)) (setq heading "[No Title]"))
              (replace-match link t t)
              (if (stringp code)
                  (if (string-match (concat "^" lookup-url-regexp "$")
                                    code)
                      ;; If code matches URL, then....
                      (setq entry (lookup-new-entry 'url dict code heading))
                    (setq entry (lookup-new-entry 'regular dict code heading)))
                (setq entry (lookup-new-entry 'dynamic dict heading))
                (lookup-put-property entry :dynamic code))
              (lookup-set-link start (point) entry))))))))

(defun lookup-arrange-references-url (entry)
  (goto-char (point-min))
  (let ((dict (lookup-entry-dictionary entry)))
    (while (re-search-forward lookup-url-regexp nil t)
      (let ((match-string (match-string 0)))
        (lookup-set-link (match-beginning 0) (match-end 0)
                         (lookup-new-entry 'url dict match-string 
                                           match-string))))))

(defun lookup-adjust-check-references (_entry)
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

(defun lookup-adjust-show-gaijis (_entry)
  (when lookup-enable-gaiji
    (lookup-map-over-property
     (point-min) (point-max) 'lookup-gaiji
     (lambda (start end gaiji)
       (lookup-gaiji-glyph-paste start end (lookup-gaiji-glyph gaiji))))))

;; fill

(defun lookup-arrange-nofill (_entry))

(defun lookup-arrange-fill-lines (_entry)
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

;;(defun lookup-arrange-fill-paragraphs (entry)
;;  (text-mode)
;;  (let ((fill-column (if (integerp lookup-fill-column)
;;                         lookup-fill-column
;;                       (round (* (window-width) lookup-fill-column)))))
;;    (fill-individual-paragraphs (point-min) (point-max))))

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
  (or lookup-select-module
      (let ((session (lookup-current-session)))
        (if session (lookup-session-module session)
          (lookup-default-module)))))

(defun lookup-default-module ()
  "Default module of current buffer."
  (let ((name (or (lookup-assq-get lookup-mode-module-alist major-mode)
                  (lookup-assq-get lookup-mode-module-alist t))))
    (if name
        (or (lookup-get-module name)
            (error "No such module: %s" name))
      (car lookup-module-list))))

(defun lookup-get-module (name &optional module-list)
  (car (cl-member-if (lambda (module) (equal (lookup-module-name module) name))
                     (or module-list lookup-module-list))))

(defun lookup-get-agent (id)
  (car (cl-member-if (lambda (agent) (equal (lookup-agent-id agent) id))
                     lookup-agent-list)))

(defun lookup-get-dictionary (id)
  (car (cl-member-if (lambda (dict) (equal (lookup-dictionary-id dict) id))
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

(defun lookup-get-buffer (name)
  (let ((buffer (get-buffer-create name)))
    (setq lookup-buffer-list (cl-adjoin buffer lookup-buffer-list))
    buffer))

(defun lookup-pop-to-buffer (&optional buffer)
  ;; BUFFER becomes current buffer in Emacs 24.2 and later.
  (setq buffer (or buffer (current-buffer)))
  (if (window-live-p lookup-main-window)
      (progn
        ;; select-window function on Emacs 24.2 and later switches
        ;; current buffer to window's buffer.
        (set-window-buffer lookup-main-window buffer)
        (select-window lookup-main-window)
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
  (when (buffer-live-p buffer) (bury-buffer buffer)))

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

(defun lookup-set-agent-options (id &rest options)
  (declare (indent 1))
  (let ((plist (lookup-assoc-get lookup-agent-option-alist id)))
    (while options
      (setq plist (plist-put plist (car options) (cadr options)))
      (setq options (cddr options)))
    (lookup-assoc-set 'lookup-agent-option-alist id plist)))

(defun lookup-set-dictionary-options (id &rest options)
  (declare (indent 1))
  "Set dictionary ID's OPTIONS plist prior to dictionary initialization."
  (let ((plist (lookup-assoc-get lookup-dictionary-option-alist id)))
    (while options
      (setq plist (plist-put plist (car options) (cadr options)))
      (setq options (cddr options)))
    (lookup-assoc-set 'lookup-dictionary-option-alist id plist)))

(defun lookup-use-support (id file)
  (lookup-assoc-set 'lookup-support-alist id file))


;;;
;;; Initialize Lookup
;;;

(defun lookup-initialize ()
  (with-temp-buffer
    (lookup-splash)
    (load lookup-init-file t)
    (setq lookup-search-modules
	  (if (and (lookup-load-cache) lookup-module-attributes)
	      (mapcar 'list (mapcar 'car lookup-module-attributes))
	    lookup-search-modules))
    (setq lookup-search-history (lookup-new-history))
    (setq lookup-agent-list
          (mapcar (lambda (spec) (apply 'lookup-new-agent spec))
                  (or lookup-search-agents
                      (setq lookup-search-agents '((ndtut))))))
    (setq lookup-dictionary-list
          (apply 'append
                 (mapcar 'lookup-agent-dictionaries lookup-agent-list)))
    (setq lookup-module-list
          (mapcar (lambda (spec) (apply 'lookup-new-module spec))
                  (or lookup-search-modules '(("default" t)))))
    (lookup-init-support-autoload)
    (run-hooks 'lookup-load-hook)
    (add-hook 'kill-emacs-hook 'lookup-exit)))

(defun lookup-clear ()
  "Clear all related variables without calling :kill command to
dictionaries."
  (setq lookup-buffer-list nil)
  (setq lookup-agent-list nil)
  (setq lookup-module-list nil)
  (setq lookup-dictionary-list nil)
  (setq lookup-entry-table nil)
  (setq lookup-current-session nil)
  (setq lookup-search-history nil)
  (setq lookup-last-session nil)
  (remove-hook 'kill-emacs-hook 'lookup-exit))

(defun lookup-init-support-autoload ()
  (load "support-defs")
  (dolist (pair (append lookup-support-autoload-alist 
                        lookup-support-autoload-default-alist))
    (dolist (dict lookup-dictionary-list)
      (when (string-match (car pair) (lookup-dictionary-id dict))
        (lookup-use-support (lookup-dictionary-id dict)
                            (cdr pair))))))

(eval-and-compile
  (defvar lookup-splash-file-name "/lookup-logo.png")

  (defvar lookup-splash-image
    (eval-when-compile
      (let ((file (concat "." lookup-splash-file-name)))
        (when (and (boundp 'lookup-byte-compiling)
                   lookup-byte-compiling
                   (file-exists-p file))
          (with-temp-buffer
            (insert-file-contents-literally file)
            (buffer-string)))))))

(defun lookup-splash ()
  "Display splash scrren in current buffer, if supported."
  (let* ((image-file (concat (file-name-directory (locate-library "lookup"))
                             lookup-splash-file-name))
         (filep (file-exists-p image-file)))
    (when (and lookup-enable-splash
               (image-type-available-p 'png)
               (or filep lookup-splash-image))
      (erase-buffer)
      (display-buffer (current-buffer))
      (let ((img (create-image (if filep image-file lookup-splash-image)
                               'png (null filep)))
            (fill-column (window-width)))
        (insert (propertize " " 'display
                            `(space :align-to (+ center (-0.5 . ,img)))))
        (insert-image img)
        (insert "\n\n")
        (insert (format "\nLookup %s\n\n" lookup-version))
        (insert "Copyright (C) 1999-2013 Lookup Development Team\n")
        (center-region (point-min) (point))
        (goto-char (point-min))
        (insert-char ?\n (max 1 (/ (- (window-height)
                                      (count-lines (point-min) (point-max)) 6)
                                   2)))
        (sit-for 1)))))

(provide 'lookup)

(unless lookup-byte-compiling
  (lookup-initialize))

;;; lookup.el ends here
