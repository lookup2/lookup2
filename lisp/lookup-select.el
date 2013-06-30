;;; lookup-select.el --- Lookup select mode -*- lexical-binding: t -*-
;; Copyright (C) 2000,2009 Lookup Development Team

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Modified by: Taichi Kawabata <kawabata.taichi@gmail.com>
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

(require 'cl-lib)
(require 'lookup-types)
(declare-function lookup-current-module "lookup")
(declare-function lookup-get-buffer "lookup")
(declare-function lookup-pop-to-buffer "lookup")
(declare-function lookup-display-buffer "lookup")
(declare-function lookup-display-entries "lookup")
(declare-function lookup-search-pattern "lookup")
(declare-function lookup-input-dictionary "lookup")
(declare-function lookup-init-support-autoload "lookup")
(declare-function lookup-get-dictionary "lookup")

(defconst lookup-select-priority-marks
  '((t . ?*) (secondary . ?$) (supplement . ?+) (nil . ? )))

(defvar lookup-select-module nil)

;;;###autoload
(defun lookup-select-dictionaries (module)
  (interactive (list (lookup-current-module)))
  (with-current-buffer (lookup-get-buffer " *Dictionary List*")
    (setq lookup-select-module module)
    (lookup-select-mode)
    (lookup-select-build-buffer)
    (setq buffer-undo-list nil)
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (lookup-select-goto-first)
    (lookup-pop-to-buffer)))

(defun lookup-select-build-buffer ()
  (let ((inhibit-read-only t)
        (module lookup-select-module))
    (erase-buffer)
    (insert "Lookup (module: " (lookup-module-name module) ")\n")
    (insert "Type `m' to select, `u' to unselect, `q' to leave, "
            "`?' for help.\n\n")
    (lookup-table-insert
     "%c %-15t %-18t %s\n"
     (append
      '((?% "Dictionary ID" "Title" "Methods")
        (?- "-------------" "-----" "-------"))
      (mapcar (lambda (dict)
                (let ((prio (lookup-module-dictionary-priority module dict))
                      (methods (lookup-dictionary-methods dict)))
                  (list (lookup-assq-get lookup-select-priority-marks prio)
                        (lookup-dictionary-id dict)
                        (lookup-dictionary-title dict)
                        (concat (mapcar (lambda (pair)
                                          (if (memq (car pair) methods)
                                              (cdr pair)
                                            ?.))
                                        lookup-search-method-marks)))))
              (lookup-module-dictionaries module))))))

(defun lookup-select-update-buffer ()
  (let ((line (lookup-current-line)))
    (lookup-select-build-buffer)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;
;;; Select Mode
;;;

(defconst lookup-select-mode-help
  "Lookup Select mode:

`m' - select   `u' - unselect   `$' - secondary   `+' - supplement

`a'(dd)    - add a dictionary     `C-k'    - remove this dictionary
`A'(dd)    - add all dictionaries `C-y'    - yank a removed dictionary
`c'(reate) - create a module

`f'(ind)   - search pattern       `F'(ind)    - search this dictionary
`i'(nfo)   - dictionary info      `M'(enu)    - show dictionary menu
`r'(eload) - reload settings.     `A'(dd all) - add all missing dictionaries

`r' - return  `q' - leave  `g' - reset  `Q' - quit  `R' - restart
`^' - modules

Search Methods:

`=' - exact  `>' - prefix  `<' - suffix `-' - substring `*' - wildcard
`%' - regexp `@' - keyword `/' - text   `I' - index     `M' - menu")

(defvar lookup-select-mode-map nil
  "*Keymap for Lookup Select mode.")

(unless lookup-select-mode-map
  (setq lookup-select-mode-map (make-sparse-keymap))
  (set-keymap-parent lookup-select-mode-map lookup-global-map)
  ;; general commands
  (define-key lookup-select-mode-map " " 'next-line)
  (define-key lookup-select-mode-map "n" 'next-line)
  (define-key lookup-select-mode-map "p" 'previous-line)
  ;; select dictionary
  (define-key lookup-select-mode-map "m" 'lookup-select-dictionary-select)
  (define-key lookup-select-mode-map "\em" 'lookup-select-dictionary-select-all)
  (define-key lookup-select-mode-map "u" 'lookup-select-dictionary-unselect)
  (define-key lookup-select-mode-map "U" 'lookup-select-dictionary-unselect-all)
  (define-key lookup-select-mode-map "$" 'lookup-select-dictionary-secondary)
  (define-key lookup-select-mode-map "+" 'lookup-select-dictionary-supplement)
  ;; dictionary information
  (define-key lookup-select-mode-map "i" 'lookup-select-dictionary-info)
  (define-key lookup-select-mode-map "M" 'lookup-select-dictionary-menu)
  (define-key lookup-select-mode-map "F" 'lookup-select-dictionary-search)
  ;; dictionary management
  (define-key lookup-select-mode-map "a" 'lookup-select-add-dictionary)
  (define-key lookup-select-mode-map "A" 'lookup-select-add-all-dictionaries)
  (define-key lookup-select-mode-map "\ey" 'lookup-select-wrap-command)
  (define-key lookup-select-mode-map "\C-k" 'lookup-select-wrap-command)
  (define-key lookup-select-mode-map "\C-y" 'lookup-select-wrap-command)
  (define-key lookup-select-mode-map "\C-x\C-t" 'lookup-select-wrap-command)
  (define-key lookup-select-mode-map [?\C-/] 'lookup-select-wrap-command)
  ;; modules
  (define-key lookup-select-mode-map "c" 'lookup-modules-create-module)
  (define-key lookup-select-mode-map "^" 'lookup-list-modules)
  ;; general commands
  (define-key lookup-select-mode-map "g" 'lookup-select-update)
  (define-key lookup-select-mode-map "q" 'lookup-leave)
  (define-key lookup-select-mode-map "r" 'lookup-select-reload-settings))

(defvar lookup-select-mode-hook nil
  "*Hook for Lookup select mode.")

(defvar lookup-select-kill-ring nil)

(defun lookup-select-mode ()
  "\\{lookup-select-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'lookup-select-mode)
  (setq mode-name "Select")
  (setq mode-line-buffer-identification
        '("Lookup:%12b <" (lookup-module-name lookup-select-module) ">"))
  (setq lookup-help-message lookup-select-mode-help)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map lookup-select-mode-map)
  (run-hooks 'lookup-select-mode-hook))

;;;
;;; Interactive Commands
;;;

(defun lookup-select-dictionary-select ()
  "Select the dictionary on the current line.
A \"selected\" dictionary will be used whenever a search is conducted."
  (interactive)
  (lookup-select-dictionary-set-priority t))

(defun lookup-select-dictionary-select-default ()
  "Select the dictionary on the current line.
Its priority will be determined by dictionary option `:priority'."
  (interactive)
  (lookup-select-dictionary-set-priority 'default))

(defun lookup-select-dictionary-select-all ()
  "Select the dictionary on the all line."
  (interactive)
  (save-excursion
    (lookup-select-goto-first)
    (while (lookup-select-this-dictionary)
      (lookup-select-dictionary-set-priority 'default))))

(defun lookup-select-dictionary-unselect ()
  "Unselect the dictionary on the current line.
An \"unselected\" dictionary will never be used in usual, but may
be selected later.  An unselected dictionary can also be used
by the command `\\[lookup-select-dictionary-search]'."
  (interactive)
  (lookup-select-dictionary-set-priority nil))

(defun lookup-select-dictionary-unselect-all ()
  "Unselect all dictionaries."
  (interactive)
  (save-excursion
    (lookup-select-goto-first)
    (while (lookup-select-this-dictionary)
      (lookup-select-dictionary-set-priority nil))))

(defun lookup-select-dictionary-secondary ()
  "Select the dictionary on the current line as a secondary dictionary.
A \"secondary\" dictionary will be used only when the other (\"primary\")
dictionaries could not find any entries."
  (interactive)
  (lookup-select-dictionary-set-priority 'secondary))

(defun lookup-select-dictionary-supplement ()
  "Select the dictionary on the current line as a supplement dictionary.
A \"supplement\" dictionary will be used only when the other dictionaries
have found some entries, which means this dictionary cannot appear alone."
  (interactive)
  (lookup-select-dictionary-set-priority 'supplement))

(defun lookup-select-dictionary-set-priority (value)
  (let ((dict (lookup-select-this-dictionary)))
    (when dict
      (if (equal value 'default)
          (setq value (or (lookup-dictionary-option dict :priority t) t)))
      (setf (lookup-module-dictionary-priority lookup-select-module dict)
            value)
      (lookup-select-set-mark
       (lookup-assq-get lookup-select-priority-marks value)))))

(defun lookup-select-dictionary-info ()
  (interactive)
  (cl-flet ((plist-to-string-pair 
             (plist)
             (mapcar (lambda (item)
                       (list (format "%s" (car item))
                             (replace-regexp-in-string 
                              "\n" "\\\\n"
                              (format "%S" (cdr item)))))
                     (lookup-plist-to-alist plist))))
    (let* ((dictionary (lookup-select-this-dictionary))
           (dictionary-options (lookup-dictionary-options dictionary))
           (agent (lookup-dictionary-agent dictionary))
           (agent-options (lookup-agent-options agent))
           ;(priority (lookup-module-dictionary-priority 
           ;           lookup-select-module dictionary))
           )
      (with-current-buffer (lookup-get-buffer "*Dictionary Information*")
        (let ((inhibit-read-only t))
          (help-mode)
          (erase-buffer)
          (insert "Dictionary Options Information\n\n")
          (insert " - Dictionary ID: " (lookup-dictionary-id dictionary) "\n")
          (insert " - Agent ID:      " (lookup-agent-id agent) "\n\n")
          (lookup-table-insert
           " %-15t %-18s\n"
           (append
            '(("Dictionary Options" "")
              ("Option Name       " "Value")
              ("------------------" "-----"))
            (plist-to-string-pair dictionary-options)
            '(("" "")
              ("Agent Options     " "")
              ("Option Name       " "Value")
              ("------------------" "-----"))
            (plist-to-string-pair agent-options))))
        (goto-char (point-min))
        (lookup-display-buffer (current-buffer))))))

(defun lookup-select-dictionary-menu (args)
  "Display selected dictionary menu.
With prefix ARGS, display menus of all dictionaries in current module ."
  (interactive "P")
  (let* ((module lookup-select-module)
         (dicts (if args (lookup-module-dictionaries module)
                  (list (lookup-select-this-dictionary))))
         entries)
    (dolist (dict dicts)
      (if (memq 'menu (lookup-dictionary-methods dict))
          (setq entries (append (lookup-dictionary-menu dict) entries))))
    (if entries
        (let ((query (lookup-new-query 'reference "Menu")))
          (lookup-display-entries module query (nreverse entries)))
      (error "No dictionary has a menu"))))

(defun lookup-select-dictionary-search (pattern &optional max-hits)
  "Search the dictionary on the current line for PATTERN.
Only the dictionary at point will be used regardless of states of
other dictionaries.  With prefix-argument, MAX-HITS can be specified."
  (interactive
   (let ((dict (lookup-select-this-dictionary)))
     (if dict
         (nreverse
         (list (when current-prefix-arg
                 (string-to-number (lookup-read-string "Max Hits")))
               (lookup-read-string
                (format "Look up by `%s'" (lookup-dictionary-title dict))
                nil 'lookup-input-history)))
       (error "No dictionary at the current line"))))
  (let ((lookup-search-dictionaries (list (lookup-select-this-dictionary)))
        (lookup-max-hits (or max-hits lookup-max-hits))
        (lookup-force-update (and max-hits t)))
    (lookup-search-pattern lookup-select-module pattern)))

(defun lookup-select-add-dictionary (dictionary)
  "Add a DICTIONARY into the current module."
  (interactive (list (lookup-input-dictionary lookup-select-module)))
  (let* ((module lookup-select-module)
         (dict (lookup-select-this-dictionary))
         (dicts (lookup-module-dictionaries module)))
    (if (eq dict (car dicts))
        (setf (lookup-module-dictionaries module) (cons dictionary dicts))
      (while (not (eq dict (cadr dicts))) (setq dicts (cdr dicts)))
      (setcdr dicts (cons dictionary (cdr dicts)))))
  (lookup-select-update-buffer))

(defun lookup-select-add-all-dictionaries ()
  "Add all dictionaries into the current module at the end."
  (interactive)
  (let* ((module lookup-select-module)
         (dicts (lookup-module-dictionaries module))
         (diffs
          (cl-set-difference lookup-dictionary-list dicts)))
    (if diffs
        (setf (lookup-module-dictionaries module)
              (nconc dicts (nreverse diffs)))))
  (lookup-select-update-buffer))

(defun lookup-select-wrap-command (_arg)
  "Call the corresponding global command with keys and reset dictionaries.
This command should be bound to the same keys with commands
`kill-line', `yank', `yank-pop',`transpose-lines', or `undo'.
When this command is called, variable `lookup-select-kill-ring'
will be used instead of the usual `kill-ring'."
  (interactive "P")
  (let ((kill-ring lookup-select-kill-ring)
        (kill-whole-line t)
        (inhibit-read-only t))
    (beginning-of-line)
    (unwind-protect
        (progn
          (use-local-map global-map)
          (call-interactively (key-binding (this-command-keys))))
      (use-local-map lookup-select-mode-map))
    (setq lookup-select-kill-ring kill-ring)
    (lookup-select-reset-dictionaries)))

(defun lookup-select-update ()
  (interactive)
  (let* ((module lookup-select-module)
         (message (format "Updating %s..." (lookup-module-name module))))
    (message message)
    ;; (dolist (dict (lookup-module-dictionaries module))
    ;;   (lookup-dictionary-setplist dict nil))
    (lookup-select-update-buffer)
    (message (concat message "done"))))

(defun lookup-select-reload-settings ()
  (interactive)
  (let ((dict (lookup-select-this-dictionary)))
    (lookup-init-support-autoload)
    (lookup-dictionary-initialize dict)))

;;;
;;; Internal functions
;;;

(defun lookup-select-goto-first ()
  "Set point to the beginning of the first dictionary line."
  (goto-char (point-min))
  (forward-line 5))

(defun lookup-select-set-mark (mark)
  "Set MARK for the current line dictionary."
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (delete-char 1)
    (insert-char mark 1))
  (forward-line))

(defun lookup-select-this-dictionary ()
  "Return the current line dictionary or nil."
  (save-excursion
    (beginning-of-line)
    (goto-char (+ (point) 2))
    (let ((lookup-property (plist-get (text-properties-at (point)) 'lookup)))
      (and lookup-property 
           (lookup-get-dictionary (elt lookup-property 1))))))

(defun lookup-select-reset-dictionaries ()
  "Reset the current module dictionaries with their priorities."
  (save-excursion
    (lookup-select-goto-first)
    (let ((module lookup-select-module) dict dicts)
      (while (setq dict (lookup-select-this-dictionary))
        (setf (lookup-module-dictionary-priority module dict)
              (car (rassq (char-after (point))
                          lookup-select-priority-marks)))
        (setq dicts (cons dict dicts))
        (forward-line))
      (setf (lookup-module-dictionaries module) (nreverse dicts)))))

(provide 'lookup-select)

;;; lookup-select.el ends here
