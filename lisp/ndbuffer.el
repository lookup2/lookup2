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
(defun ndbuffer-dictionary-methods (dictionary)
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
                    (lookup-dictionary-agent dictionary))))
         entries)
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
            (content-tags entry-tags code-tags head-tags entry-tags-list)
            (ndtext-dictionary-options dictionary heading)
          (ndtext-process 'ndbuffer 'get file code 'exact
                          content-tags entry-tags head-tags
                          code-tags)))))

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

(defun ndbuffer-search (file string method conent-tags entry-tags head-tags code-tags)
  "Extract entries in FILE with STRING, METHOD, ENTRY-START and ENTRY-END.
REGULAR indicates if search is regular (not used for now).
Extractions are done in accordance with follows."
  (let ((regexp (ndbuffer-regexp string method content-tags entry-tags))
        (content-end   (cdr content-tags))
        (head-start    (regexp-quote (car head-tags)))
        (head-end      (regexp-quote (cdr head-tags)))
        (code-start    (regexp-quote (car code-tags)))
        (code-end      (regexp-quote (cdr code-tags)))
        result)
    (with-current-buffer (find-file-noselect file)
      (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((start (match-beginning 0)) end)
          (or (search-forward content-end nil t)
              (goto-char (point-max)))
          ;; stop before `content-end'.
          (setq end (match-beginning 0))
          (save-restriction
            (narrow-to-region (match-beginning 0) 
			      (- (match-end 0) (length content-end)))
            (goto-char (point-min))
            (let (code head val)
              (when (re-search-forward (concat head-start "\\(.+?\\)" 
                                               head-end) nil t)
                (setq head (match-string 1)))
              (goto-char (point-min))
              (when (re-search-forward (concat code-start "\\(.+?\\)" 
                                               code-end) nil t)
                (setq code (match-string 1)))
              (when (and code head)
                (push (list code head (buffer-substring
                                       (match-beginning 0) (match-end 0)))
                      result)))
            (goto-char (point-max)))))))))

(defun ndbuffer-file-content (file code content-tags code-tags)
  "Get substring of FILE containing CODE, inside CONTENT-TAGS."
  (let ((regexp (ndbuffer-regexp string method content-tags code-tags))
        (content-end   (cdr content-tags))
        (head-start    (regexp-quote (car head-tags)))
        (head-end      (regexp-quote (cdr head-tags)))
        (code-start    (regexp-quote (car code-tags)))
        (code-end      (regexp-quote (cdr code-tags)))
        result)
    (with-current-buffer (find-file-noselect file)
      (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (let ((start (match-beginning 0)) end)
            (or (search-forward content-end nil t)
                (goto-char (point-max)))
            (buffer-substring start (point))))))))

(defun ndbuffer-regexp (string method content-tags tags)
  "Construct regular expression.
cf. `ndtext-multi-line-pattern'."
;; <start>\\(.*search.*\\)</end>
  (let ((entry-start   (regexp-quote (car entry-tags)))
        (entry-end     (regexp-quote (cdr entry-tags)))
        (content-start (regexp-quote (car content-tags)))
        (content-end   (regexp-quote (cdr content-tags)))
        (any-char      (concat "\\(?:.\\|\\n\\)")))
    (concat
     content-start      ; <content> 
     (if entry-start
         ;; * <entry> is defined
         ;;   <content>..?(search-word)..(</content>)
         ;;              exact     :: <entry>string</entry>
         ;;              prefix    :: <entry>string
         ;;              suffix    :: string</entry>
         ;;              substring :: string
         ;;              text      :: </entry>..?string
         (concat any-char "?"
                 (case method
                   ('exact
                    (concat entry-start string entry-end))
                   ('prefix
                    (concat entry-start string))
                   ('suffix
                    (concat string entry-end))
                   (t string)))
       ;; * <entry> is not defined
       ;;   <content>(search-word)..(</content>)
       ;;              exact     :: string</entry>
       ;;              prefix    :: string
       ;;              suffix    :: ..?string</entry>
       ;;              substring :: ..?string
       ;;              text      :: ..?</entry>..string
       ;;    ※ ".." = (!</content)*
       (case method
         ('exact (concat string entry-end))
         ('prefix string)
         ('suffix (concat any-char "?" entry-end))
         (t (concat any-char "?" string)))))))
     ;; any-char))) ;; after this point, another search will be established.
  
(provide 'ndbuffer)

;;; ndbuffer.el ends here
