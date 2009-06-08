;;; ndsary.el --- Lookup `sary' interface -*- coding: utf-8 -*-

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

;; ndsary.el provides the suffix array index searching capability
;; using `sary' program (http://www.namazu.org/sary/).
;;
;; Typical text dictionary look as follows:
;;
;; ....
;; <content-start>
;;  <code-start>CODE</code-end>
;;  <head-start>HEAD</head-end>
;;  <entry-start>ENTRY1</entry-end>
;;  <entry-start>ENTRY2</entry-end>
;;  ....
;;  <entry-start-2>ENTRY2-1</entry-end-2>
;;  <entry-start-2>ENTRY2-2</entry-end-2>
;;  ....
;;  <explanation>....</explanation>
;; </content-end>
;; ....
;;
;; All the above tags are optional.
;;
;; If `:entry-tags' or `:entry-tags-list' is not provided, then only
;; `exact' and `prefix' search is possible, where searched keyword
;; itself will be a CODE.  `:entry-tags' or `:entry-tags-list' may be
;; a function with arguments of queried string and method.
;;
;; If `:entry-tags' is provided by the dictionary spec, then `exact',
;; `prefix', `suffix', `substring' and `text' search is possible.
;;
;; If `:content-tags' are not provided, then each LINE is assumed to
;; be each content.  `:content-tags' may be a function, whose argument
;; is `entry-tags' cons cell (for searching) or `code' string (for
;; content display).
;;
;; If `:code-tags' are not provided, then entire concatenation of
;; `entry-start'+entry+`entry-end' will be a code.
;;
;; If `head-tags' are not provided, then substring surrounded by
;; entry-start and entry-end will be a head.  `head-tags' may be a
;; function, whose argument is `string' (for entry).
;;
;; While `substring' method only matches the string between
;; entry-start and entry-end, `text' method matches anywhere in the
;; content.
;;
;; If `content-start' is a natural number , then that number of line
;; before the hit line will be provided as content.  If `content-end'
;; is a natural number, then that number of following lines will be
;; provided as a content.
;;
;; If multiple contents are found by single CODE, then all the contents
;; will be concatenated to form one single content.

;;; Usage:
;;
;;  Specify the directory with XXX.ary files.  If you want to specify
;;  the specs common to all dictionaries in the folder, specify as so.
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/wikipedia"
;;            :entry-start "<title>Wikipedia: " 
;;            :entry-end "</title>"
;;            :content-start "<doc>" :content-end "</doc>"
;;            :arranges ((replace remove-xml-tag-entry))
;;            )
;;           ....
;;           ))
;;
;;  If you want to give multiple search methods to single XML file, 
;;  then please make a hard-link copy of them and create purpose-specific
;;  ".ary" files individually.

;;; Code:

(require 'lookup)


;;;
;;; Customizable Variables
;;;

;;(defgroup ndsary nil
;;  "Lookup ndsary interface."
;;  :group 'lookup-agents)

(defvar ndsary-sary-program "sary")
(defvar ndsary-sary-program-options '("-i"))

;;;
;;; Internal variables
;;;

(defvar ndsary-default-dict-specs
  '((:entry-start nil :entry-end nil
     :content-start nil :contents-end nil)))

;;;
;;; Interface functions
;;;

(put 'ndsary :methods 'ndsary-dictionary-methods)
(defun ndsary-dictionary-methods (dictionary)
  '(exact prefix suffix substring text))

(put 'ndsary :list 'ndsary-list)
(defun ndsary-list (agent)
  "Return list of dictionaries of AGENT."
  (let* ((files (directory-files 
                 (expand-file-name (lookup-agent-location agent))
                 nil "\\.ary\\'")))
    (mapcar (lambda (name) 
              (lookup-new-dictionary agent (file-name-sans-extension name)))
            files)))

(put 'ndsary :title 'ndsary-title)
(defun ndsary-title (dictionary)
  "Get title of DICTIONARY."
  (or (lookup-dictionary-option dictionary :title)
      (let ((name (lookup-dictionary-name dictionary)))
        (file-name-sans-extension name))))

(put 'ndsary :search 'ndsary-dictionary-search)
(defun ndsary-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let ((entry-tags      (lookup-dictionary-option dictionary :entry-tags t))
        (entry-tags-list (lookup-dictionary-option dictionary :entry-tags-list t))
        (content-tags    (lookup-dictionary-option dictionary :content-tags t))
        (code-tags       (lookup-dictionary-option dictionary :code-tags t))
        (head-tags       (lookup-dictionary-option dictionary :head-tags t))
        (string      (lookup-query-string query))
        (method      (lookup-query-method query))
        (file        (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
        (coding      (or (lookup-dictionary-option dictionary :coding t)
                         'utf-8))
        (max-hits    (or (lookup-dictionary-option dictionary :max-hits t)
                         lookup-max-hits))
        entries)
    (if (functionp code-tags)
        (setq code-tags (apply code-tags '(search))))
    (if (null entry-tags-list)
        (setq entry-tags-list (list entry-tags)))
    (setq entries
          (lookup-with-coding-system coding
            (ndsary-file-searches
             file string method entry-tags-list
             content-tags code-tags head-tags)))
    (mapcar
     (lambda (x) (lookup-new-entry
                  'regular dictionary (car x) (cdr x)))
     entries)))

(put 'ndsary :content 'ndsary-entry-content)
(defun ndsary-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((code-tags    (or (lookup-dictionary-option dictionary :code-tags t)
                            (lookup-dictionary-option dictionary :entry-tags t)))
         (content-tags (lookup-dictionary-option dictionary :content-tags t))
         (code         (lookup-entry-code entry))
         (dictionary   (lookup-entry-dictionary entry))
         (coding       (or (lookup-dictionary-option dictionary :coding t)
                           'utf-8))
         (file         (expand-file-name
                        (lookup-dictionary-name dictionary)
                        (lookup-agent-location
                         (lookup-dictionary-agent dictionary)))))
    (if (functionp code-tags)
        (setq code-tags (apply code-tags '(content))))
    (setq code (concat (car code-tags) code (cdr code-tags)))
    (if (functionp content-tags) 
        (setq content-tags (apply content-tags (list code))))
    (lookup-with-coding-system coding
      (ndsary-file-content file code
                           (car content-tags) (cdr content-tags) coding))))

;;;
;;; Main Program
;;;

;;;###autoload
(defun ndsary-file-searches
  (file string method entry-tags-list &optional 
   content-tags code-tags head-tags)
  "Return entry list of FILE STRING of METHOD for START-END-PAIRS.
For the rest of arguments, please refer `ndsary-file-search'."
  (if (functionp entry-tags-list)
      (setq entry-tags-list 
            (apply entry-tags-list (list string method))))
  (remove-duplicates
   (apply 'nconc
          (mapcar (lambda (x)
                    (ndsary-file-search
                     file string method x
                     content-tags code-tags head-tags))
                  entry-tags-list))
   :test (lambda (x y) (equal (car x) (car y)))))

;;;###autoload
(defun ndsary-file-search
  (file string method &optional entry-tags
   content-tags code-tags head-tags)
  "Return entry list in FILE for STRING with METHOD.
ENTRY-TAGS specifies entry.  Returned list will be a list
of (code . heading).  If CONTENT-TAGS is provided, the region
surrounded by CONTENT-TAGS will be searched for CODE and HEAD."
  ;; Note: If ENTRY-TAGS contains newlines, then it is suggested to
  ;; provide CONTENT-TAGS to properly extract entries.
  (if (functionp content-tags) 
      (setq content-tags (apply content-tags (list entry-tags))))
  (let* ((pattern 
          (ndsary-pattern string method (car entry-tags) (cdr entry-tags)))
         (count 0) entries)
    (if (/= 0 lookup-max-hits)
        (setq count (ndsary-file-match-count file pattern)))
    (if (and (/= 0 lookup-max-hits) (< lookup-max-hits count))
        (setq entries (list
                       (cons "�" (format "Error. More than %s hits. (%d)" 
                                         lookup-max-hits count))))
      (if (= count 0) (setq entries nil)
        ;; count /= 0
        (if (null entry-tags)
            ;; TODO method `exact' vs `prefix'
            (setq entries (list (cons string string)))
          (setq entries 
                (mapcar (lambda (x) 
                          (ndsary-extract-entry 
                           x string method entry-tags code-tags head-tags))
                        (ndsary-file-content file pattern 
                                             (car (or content-tags entry-tags))
                                             (cdr (or content-tags entry-tags))
                                             'split))))
        (setq entries (remove-if 'null entries))))
    entries))

(defun ndsary-pattern (string method start end)
  "Costruct search pattern from query STRING and METHOD for `sary'.
If START tag is provided, then that will be attached.
If END tag is provided, then that will also be attached."
  ;;(if (and (or (null start) (null end))
  ;;         (or (equal method 'suffix)
  ;;             (equal method 'substring)
  ;;             (equal method 'text)))
  ;;    nil
    (concat (if (or (equal method 'exact)
                    (equal method 'prefix))
                start)
            string
            (if (or (equal method 'exact)
                    (equal method 'suffix))
                end)));;)

(defun ndsary-extract-entry
  (entry string method entry-tags &optional code-tags head-tags)
  "Check and extract code and head for ENTRY.
If STRING is not surrounded by ENTRY-TAGS, nil will be returned.
If CODE-TAGS is null, ENTRY surrounded by ENTRY-TAGS will be a code.
If CODE-TAGS is a function, result applied to ENTRY will be a code.
If HEAD-TAGS is null, CODE will be a head.
If HEAD-TAGS is a function, result applied to ENTRY will be a head."
  (let ((entry-start (regexp-quote (car entry-tags)))
        (entry-end   (regexp-quote (cdr entry-tags))))
    (if (and (or (equal method 'substring) (equal method 'prefix)
                 (equal method 'suffix))
             (not (string-match (concat entry-start ".*?"
                                        (regexp-quote string) ".*?"
                                        entry-end) entry)))
        nil ;; invalid
      (let* ((code (if (null code-tags)
                       (progn
                         (string-match (concat entry-start "\\(.*?\\)" entry-end) entry)
                         (match-string 0 entry))
                     (if (functionp code-tags) (apply code-tags (list entry))
                       (ndsary-extract-string entry (car code-tags)
                                              (cdr code-tags)))))
             (head (if (null head-tags) 
                       (if (null code-tags) (match-string 1 entry) code)
                     (if (functionp head-tags) (apply head-tags (list entry))
                       (ndsary-extract-string entry (car head-tags) 
                                              (cdr head-tags))))))
        ;;(if (not (stringp code)) (error "Lookup error!"))
        (cons code head)))))
                      
(defun ndsary-extract-string (string start end)
  "Extract string surround by START and END from STRING.
If START is a function, then it will be applied to STRING."
  (if (functionp start) (apply start (list string))
    (if (string-match (concat (regexp-quote start)
                              "\\(.+?\\)"
                              (regexp-quote end)) string)
        (match-string 1 string))))

;;;
;;; Low Level functions
;;;

;;;###autoload
(defun ndsary-file-match-count (file pattern)
  "Return the number of match in FILE for PATTERN."
  (with-temp-buffer
    (let ((args (append ndsary-sary-program-options
                        (list "-c" pattern file))))
      (apply 'call-process
             ndsary-sary-program nil t nil args))
    (goto-char (point-min))
    (if (looking-at "\\([0-9]+\\)")
        (string-to-number (match-string 1))
      0)))

;;;###autoload
(defun ndsary-file-content 
  (file string &optional content-start content-end option)
  "Extract contents of STRING inf FILE from CONTENTS-START to CONTENTS-END.
If OPTION is `split', then split the result."
  (if (null content-start) (setq content-start "\n"))
  (if (null content-end) (setq content-end "\n"))
  (if (equal "�" string) string
    (with-temp-buffer
      (apply 'call-process ndsary-sary-program nil t nil 
             (ndsary-file-content-options file string content-start content-end))
      (if (equal option 'split)
          (if (equal content-start content-end)
              (mapcar 
               (lambda (x) (concat content-start x content-end))
               (remove-if-not (lambda (x) 
                                (string-match (regexp-quote string) x))
                              (split-string (buffer-string) content-start t)))
            (let ((regexp (concat (regexp-quote content-start)
                                  "\\|"
                                  (regexp-quote content-end)))
                  (start 1) entries)
              (goto-char (point-min))
              (while (re-search-forward regexp nil t)
                (if (equal (match-string 0) content-start)
                    (setq start (match-beginning 0))
                  ;; match-string is content-end
                  (setq entries (cons (buffer-substring start (match-end 0))
                                      entries))))
              (nreverse entries)))
        (if (equal option 'buffer)
            (current-buffer)
          (buffer-string))))))

(defun ndsary-file-content-options 
  (file string content-start content-end)
  (nconc (copy-sequence ndsary-sary-program-options)
         (if (stringp content-start) 
             (list "-s" content-start)
           (if (integerp content-start)
               (list "-A" (number-to-string content-start))))
         (if (stringp content-end) 
             (list "-e" content-end)
           (if (integerp content-end)
               (list "-B" (number-to-string content-end))))
         (list string file)))

(provide 'ndsary)

;;; ndsary.el ends here
