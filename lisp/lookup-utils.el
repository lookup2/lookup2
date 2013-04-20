;;; lookup-utils.el --- Lookup various utilities -*- lexical-binding: t -*-
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

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'lookup-vars)

;; warning.  lookup-{assq/assoc}-set functions can not be used with
;; lexically-scoped variables.  see manual of `set' function for
;; details.

;; alist by assq

(defsubst lookup-assq-get (alist key)
  (cdr (assq key alist)))

(defsubst lookup-assq-del (alist key)
  (delq (assq key alist) alist))

(defun lookup-assq-put (alist key value)
  (if value
      (cl-acons key value (lookup-assq-del alist key))
    (lookup-assq-del alist key)))

(defmacro lookup-assq-set (symbol key value)
  `(set ,symbol (lookup-assq-put (eval ,symbol) ,key ,value)))

;; alist by assoc

(defsubst lookup-assoc-get (alist key)
  (cdr (assoc key alist)))

(defsubst lookup-assoc-del (alist key)
  (delq (assoc key alist) alist))

(defun lookup-assoc-put (alist key value)
  (if value
      (cl-acons key value (lookup-assoc-del alist key))
    (lookup-assoc-del alist key)))

(defmacro lookup-assoc-set (symbol key value)
  `(set ,symbol (lookup-assoc-put (eval ,symbol) ,key ,value)))

;; multi put/get

(defun lookup-multi-get (alist &rest args)
  (lookup-multi-get-1 alist args))

(defun lookup-multi-get-1 (alist args)
  (if args
      (lookup-multi-get-1 (lookup-assq-get alist (car args)) (cdr args))
    alist))

(defun lookup-multi-put (alist &rest args)
  (lookup-multi-put-1 alist args))

(defun lookup-multi-put-1 (alist args)
  (if (cddr args)
      (lookup-assq-put alist (car args)
                       (lookup-multi-put-1 (lookup-assq-get alist (car args))
                                           (cdr args)))
    (lookup-assq-put alist (car args) (cadr args))))

;; plist to alist 

(defun lookup-plist-to-alist (plist)
  (let (alist)
    (while plist
      (push (cons (car plist) (cadr plist)) alist)
      (setq plist (cddr plist)))
    (nreverse alist)))

;;; table

(defun lookup-add-to-table (key val table)
  (let* ((old-val (typecase table
                    (hash-table (gethash key table))
                    (char-table (aref table key))
                    (symbol     (get-char-code-property key table))
                    (t (error "Improper table! %s" table))))
         (new-val (typecase old-val
                    (null val)
                    (list (if (memq val old-val) old-val (push val old-val)))
                    (t    (list old-val val)))))
    (typecase table 
      (hash-table (puthash key new-val table))
      (char-table (aset table key new-val))
      (t (put-char-code-property key new-val table)))))

;;; list thread

(defun lookup-thread-list (seq &optional seqrest)
  "Create Threaded list of SEQ."
  ;; e.g. (lookup-thread-list '((3 4) 1 (5 6)))
  ;;   -> ((3 1 5) (4 1 5) (3 1 6) (4 1 6))
  (if (null seq) seqrest
    (lookup-thread-list
     (cdr seq)
     (let ((elem (car seq)))
       (if (listp elem)
           (cl-mapcan (lambda (x)
                        (mapcar (lambda (y)
                                  (append y (list x)))
                                (or seqrest (list nil))))
                      elem)
         (mapcar (lambda (y)
                   (append y (list (car seq))))
                 (or seqrest (list nil))))))))

;; misc

(defmacro lookup-aif (cond then &rest else) ;; anaphoric if
  "Like `if', but the result of evaluating COND is bound to `it'."
  (declare (debug t) (indent 2))
  `(let ((it ,cond))
     (if it ,then ,@else)))

(defmacro lookup-with-message (msg &rest body)
  (declare (indent 1))
  `(let ((lookup-message ,msg))
     (message "%s..." lookup-message)
     (prog1 (progn ,@body)
       (message "%s...done" lookup-message))))

(defsubst lookup-temp-buffer ()
  (generate-new-buffer " *Lookup temp buffer*"))

(defun lookup-map-over-property (from to prop func &optional object)
  (let ((start from) end value)
    (while (setq value (get-text-property start prop object)
                 end (text-property-not-all start to prop value object))
      (if value (funcall func start end value))
      (setq start end))
    (if value (funcall func start to value))))

(defun lookup-current-line ()
  (save-excursion
    (do ((line 1 (1+ line)))
        ((bobp) line)
      (forward-line -1))))

(defun lookup-oneline-string (string)
  (replace-regexp-in-string "\n *" " " string))

(defun lookup-read-string (prompt &optional init history default inherit)
  (read-string (if default
                   (concat prompt " (default " default "): ")
                 (concat prompt ": "))
               init history default inherit))

(defmacro lookup-with-coding-system (coding &rest body)
  (declare (indent 1))
  `(let ((coding-system-for-read ,coding)
         (coding-system-for-write ,coding))
     ,@body))

(defmacro lookup-with-buffer-and-window (buffer &rest body)
  (declare (indent 1))
  `(let ((buffer ,buffer))
     (with-current-buffer buffer
       (save-selected-window
         (if (get-buffer-window buffer)
             (select-window (get-buffer-window buffer))
           (error "No window for buffer `%s'" buffer))
         ,@body))))

(defun lookup-table-insert (format args-list)
  (let ((format-list nil) (width-alist nil)
        (n 0) (start 0) (end 0) width)
    ;; parse format string
    (while (string-match "%\\(-?[0-9]*\\)." format start)
      (unless (eq (aref format (match-end 1)) ?%)
        (when (eq (aref format (match-end 1)) ?t)
          (setq width (string-to-number (match-string 1 format)))
          (setq width-alist (lookup-assq-put width-alist n (cons width (abs width))))
          (setq format-list
                (cons n (cons (substring format end (match-beginning 0))
                              format-list))
                end (match-end 0)))
        (setq n (1+ n)))
      (setq start (match-end 0)))
    (setq format-list (nreverse (cons (substring format end) format-list)))
    ;; get max width
    (dolist (args args-list)
      (dolist (pair width-alist)
        (setq width (string-width (nth (car pair) args)))
        (if (< (cddr pair) width)
            (setcdr (cdr pair) width))))
    ;; construct real format
    (setq format (mapconcat
                  (lambda (element)
                    (if (stringp element)
                        element
                      (let* ((pair (lookup-assq-get width-alist element))
                             (string (if (> (car pair) 0)
                                         (number-to-string (cdr pair))
                                       (number-to-string (- (cdr pair))))))
                        (concat "%" string "s"))))
                  format-list ""))
    ;; insert table
    (dolist (args args-list)
      (setq start (point))
      (insert (apply 'format format args))
      (put-text-property start (1- (point)) 'lookup args))))

(defvar lookup-property-table nil)

(defun lookup-get-property (obj key)
  (lookup-multi-get lookup-property-table obj key))

(defun lookup-put-property (obj key val)
  (setq lookup-property-table
        (lookup-multi-put lookup-property-table obj key val)))

;;;
;;; Lookup process
;;;

(defsubst lookup-open-process-buffer (name)
  (if lookup-enable-debug (generate-new-buffer name)))

(defvar lookup-process-output-start nil)
(defvar lookup-process-output-value nil)
(defvar lookup-process-output-filter nil)
(defvar lookup-process-output-finished nil)
(defvar lookup-process-output-separator nil)
(defvar lookup-process-output-separator-lines 2)

(defun lookup-process-require (process string separator &optional filter)
  (setq lookup-process-output-value nil)
  (setq lookup-process-output-filter filter)
  (setq lookup-process-output-finished nil)
  (setq lookup-process-output-separator separator)
  (let (temp-buffer)
    (unless (process-buffer process)
      (setq temp-buffer (lookup-temp-buffer))
      (set-process-buffer process temp-buffer))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert string)
      (setq lookup-process-output-start (point))
      (set-process-filter process 'lookup-process-filter)
      (process-send-string process string)
      (while (not lookup-process-output-finished)
        (unless (accept-process-output process 5)
          (when (> (point) lookup-process-output-start)
            (display-buffer (current-buffer))
            (error "Lookup BUG!! Report bug to the mailing list")))))
    (when temp-buffer
      (set-process-buffer process nil)
      (kill-buffer temp-buffer)))
  lookup-process-output-value)

(defun lookup-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (insert string)
    (forward-line (- lookup-process-output-separator-lines))
    (if (< (point) lookup-process-output-start)
        (goto-char lookup-process-output-start))
    (when (re-search-forward lookup-process-output-separator nil 0)
      (goto-char (match-beginning 0))
      (if lookup-process-output-filter
          (save-current-buffer
            (narrow-to-region lookup-process-output-start (point))
            (goto-char (point-min))
            (setq lookup-process-output-value
                  (funcall lookup-process-output-filter process))
            (widen))
        (setq lookup-process-output-value
              (buffer-substring lookup-process-output-start (point))))
      (setq lookup-process-output-finished t))))

(defun lookup-process-kill (process)
  (set-process-filter process nil)
  (delete-process process)
  (if (process-buffer process)
      (kill-buffer (process-buffer process))))

;;;
;;; simple process management utility
;;;

;; coding-system should be set by `with-lookup-coding-system'.
;; prompt, if not specified, will be treated as "\n".

;; TODO: kill-process-at-exit.

(defvar lookup-get-process-alist nil)

(defun lookup-get-process (command-args)
  "Get new process for COMMAND-ARGS.
If process is not run, silently remove the process and re-create new process."
  (let ((process (lookup-assoc-get lookup-get-process-alist command-args)))
    (unless (and process (eq (process-status process) 'run))
      (condition-case nil
          (if process (kill-process process))
        (error nil))
      (let ((buffer (lookup-open-process-buffer
                     (concat " *" (car command-args) "*"))))
        (setq process (apply 'start-process (car command-args) buffer
                             command-args))
        (set-process-query-on-exit-flag process nil)
        ;; 起動後、少し時間を置かないと、最初の検索がうまくいかない。
        (sleep-for 0.1)
        (set-process-coding-system process
                                   coding-system-for-read
                                   coding-system-for-write)
        (setq lookup-get-process-alist
              (lookup-assoc-put lookup-get-process-alist command-args process))))
    process))

(defun lookup-get-process-require (command-args string &optional prompt filter)
  (lookup-process-require 
   (lookup-get-process command-args)
   (concat string "\n") (or prompt "\n") filter))

(defun lookup-get-process-kill (&optional command-args)
  (if command-args
      (let ((process (lookup-assoc-get lookup-get-process-alist command-args)))
        (when process 
          (lookup-process-kill (lookup-get-process command-args))
          (lookup-assoc-del lookup-get-process-alist command-args)))
    (while lookup-get-process-alist
      (lookup-process-kill (cdar lookup-get-process-alist))
      (setq lookup-get-process-alist (cdr lookup-get-process-alist)))))

(provide 'lookup-utils)

;;; lookup-utils.el ends here
