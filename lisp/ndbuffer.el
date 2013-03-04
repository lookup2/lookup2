;;; ndbuffer.el --- Lookup `buffer' interface -*- lexical-binding: t -*-
;; Copyright (C) 2009 KAWABATA Taichi <kawabata.taichi@gmail.com>

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

;;; Documentation:

;; ndbuffer.el provides the buffered-file index searching capability.
;;
;; Most of the options are the same as `ndtext' program.  As of it,
;; please refer `ndtext' document for options.
;;
;; Buffer can be edited during the search, as long as it keeps
;; structure.

;;; Usage:
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndbuffer "~/edicts/rfc/" ; where rfc4949.txt exists
;;            :extension ".txt"
;;            :content-tags '("$ " . "$")
;;            :head-tags '("$ " . "\n")
;;            :extension ".txt"
;;            )
;;           ....
;;           ))

;;; Code:

(require 'lookup)
(require 'ndtext)



;;;
;;; Interface functions
;;;

(put 'ndbuffer :methods 'ndbuffer-dictionary-methods)
(defun ndbuffer-dictionary-methods (ignored)
  '(exact prefix suffix substring)) ;text))

(put 'ndbuffer :list 'ndbuffer-list)
(defun ndbuffer-list (agent)
  (setq ndtext-cache (make-hash-table :test 'equal))
  (let ((directory (lookup-agent-location agent))
        (extension (or (lookup-agent-option agent :extension)
                       ".txt"))) ; default extension is `.txt'.
    (cond
     ((not (file-directory-p directory))
      (error "ndbuffer: agent (directory) %s not found." directory))
     (t
      (mapcar (lambda (name) 
                (lookup-new-dictionary agent (file-name-nondirectory name)))
              (file-expand-wildcards (concat directory "/*" extension)))))))

(put 'ndbuffer :title 'ndbuffer-title)
(defun ndbuffer-title (dictionary)
  (or (lookup-dictionary-option dictionary :title)
      (file-name-sans-extension (lookup-dictionary-name dictionary))))

(put 'ndbuffer :search 'ndbuffer-dictionary-search)
(defun ndbuffer-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let* ((dict-id (lookup-dictionary-id dictionary))
         (string  (lookup-query-string query))
         (method  (lookup-query-method query))
         (file    (expand-file-name
                   (lookup-dictionary-name dictionary)
                   (lookup-agent-location
                    (lookup-dictionary-agent dictionary)))))
    (destructuring-bind 
        (content-tags entry-tags code-tags head-tags entry-tags-list)
        (ndtext-dictionary-options dictionary string)
      (if entry-tags (setq entry-tags-list (list entry-tags)))
      (loop for (code head val) in (ndbuffer-search-multiple
                                    file string method
                                    content-tags entry-tags-list
                                    head-tags code-tags)
            for entry = (lookup-new-entry 'regular dictionary code head)
            do (puthash (cons dict-id code) val ndtext-cache)
            collect entry))))

(put 'ndbuffer :content 'ndbuffer-entry-content)
(defun ndbuffer-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((dictionary (lookup-entry-dictionary entry))
         (dict-id    (lookup-dictionary-id dictionary))
         (heading    (lookup-entry-heading entry))
         (code       (lookup-entry-code entry)))
    (or (gethash (cons dict-id code) ndtext-cache)
        (destructuring-bind 
            (content-tags entry-tags code-tags head-tags ignored)
            (ndtext-dictionary-options dictionary heading)
          (ndbuffer-content file code content-tags entry-tags
                            head-tags code-tags)))))

;;;
;;; Main Function
;;;

(defun ndbuffer-search-multiple
  (file string method &optional content-tags entry-tags-list
   head-tags code-tags)
  (cl-remove-duplicates
   (loop for entry-tags in entry-tags-list
         nconc (ndbuffer-search file string
                                method content-tags
                                entry-tags head-tags
                                code-tags))
   :test (lambda (x y) (string= (car x) (car y)))))

(defun ndbuffer-search (file string method content-tags entry-tags head-tags code-tags)
  "Extract entries in FILE with STRING, METHOD, ENTRY-START and ENTRY-END.
REGULAR indicates if search is regular (not used for now)."
  (destructuring-bind
      (content-tags entry-tags head-tags code-tags)
      (ndtext-normalize-options string method content-tags entry-tags head-tags code-tags)
    (let* ((regexp (ndbuffer-regexp string method entry-tags))
           (content-start (regexp-quote (car content-tags)))
           (content-end   (regexp-quote (cdr content-tags)))
           (code-start    (regexp-quote (car code-tags)))
           (code-end      (regexp-quote (cdr code-tags)))
           result start end)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (save-excursion
              (or (re-search-forward content-end nil t)
                  (goto-char (point-max)))
              (setq end (point)))
            (save-excursion
              (or (re-search-backward content-start)
                  (goto-char (point-min)))
              (setq start (point)))
            (save-restriction
              (narrow-to-region start end)
              (goto-char (point-min))
              (let (code head)
                (if (functionp head-tags) 
                    (setq head (funcall head-tags (buffer-substring start end)))
                  (when (re-search-forward (concat (car head-tags) "\\(.+?\\)" 
                                                   (cdr head-tags) nil t))
                    (setq head (match-string-no-properties 1))))
                (goto-char (point-min))
                (when (re-search-forward (concat code-start "\\(.+?\\)" 
                                                 code-end) nil t)
                  (setq code (match-string-no-properties 1)))
                (when (and code head)
                  (push (list code head (buffer-substring-no-properties start end))
                        result)))
              (goto-char (point-max))))))
      result)))

(defun ndbuffer-content (file code content-tags entry-tags
                              head-tags code-tags)
  (destructuring-bind
      (content-tags ignored ignored code-tags)
      (ndtext-normalize-options code 'exact content-tags entry-tags head-tags code-tags)
    (let* ((regexp (ndbuffer-regexp code 'exact code-tags))
           (content-start (regexp-quote (car content-tags)))
           (content-end   (regexp-quote (cdr content-tags)))
           result start end)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (save-excursion
              (or (re-search-forward content-end nil t)
                  (goto-char (point-max)))
              (setq end (point)))
            (save-excursion
              (or (re-search-backward content-start)
                  (goto-char (point-min)))
              (setq start (point)))
            (push (buffer-substring-no-properties start end)
                  result)
            (goto-char (point-max)))))
      result)))

(defun ndbuffer-regexp (string method tags)
  "Construct regular expression."
  (let* ((tag-start (if (null (car tags)) "^" (regexp-quote (car tags))))
         (tag-end   (regexp-quote (cdr tags)))
         (string    (regexp-quote string))
         (any    ".*"))
    (case method
      ('exact
       (concat tag-start string tag-end))
      ('prefix
       (concat tag-start string))
      ('suffix
       (concat string tag-end))
      ('substring
       (concat tag-start any string any tag-end))
      (t string))))
  
(provide 'ndbuffer)

;;; ndbuffer.el ends here
