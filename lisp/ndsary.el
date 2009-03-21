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
;; How can text be searched depends on whether `:entry-start' and
;; `:entry-end' dictionary options are provided or not.  
;;
;; If any of them is not provided, only `exact' and `prefix' search is
;; possible.  In this case, `exact' search only hit one content, which
;; is a concatenated collection of all the matches in the text.
;; `prefix' will attempt to find out the word.  In that case, each
;; word will be an independent entry.
;;
;; If `:entry-start' and `:entry-end' is provided by the dictionary
;; spec, then `exact', `prefix', `suffix', `substring' and `keyword'
;; search is possible.
;;
;; While `substring' only matches the string between `:entry-start'
;; and `:entry-end', `keyword' matches anywhere in the content.
;;
;; If a user wants to search multiple `:entry-start' and `:entry-end'
;; pairs, then they can be provided by `:entry-start-end-pairs'
;; option instead of `:entry-start' and `entry-end'.
;;
;; If `:content-start' is not provided, then start of the line would
;; be considered as beginning of the contents.  If `:contents-end' is
;; not provided, then the end of line will be considered as the end of
;; content.  If `:content-start' is a natural number , then that
;; number of line before the hit line will be provided as content.  If
;; `:content-end' is a natural number, then that number of following
;; lines will be provided as a content.
;;
;; `:max-hits' checks the number of hit data.  If `:regular' option is
;; `t', then it is assumed that index points are only provided among
;; index regions and there is no duplicate entries.  No dual
;; `:entry-start's check or entry duplication check will be performed,
;; so that it would be a little faster.
;;
;; All the above options can also be provided as the agent option.
;; In this case, all dictionaries may inherit such options.

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
;;  If you want to give multiple search methods to singel XML file, 
;;  then please make a hard-link copy of them and create purpose-specific
;;  ".ary" files individually.

;;; Code:

(require 'lookup)


;;;
;;; 

;;(defgroup ndsary nil
;;  "Lookup ndsary interface."
;;  :group 'lookup-agents)

(defvar ndsary-sary-program "sary")

;;;
;;; Internal variables
;;;

(defvar ndsary-default-dict-specs
  '((:entry-start nil :entry-end nil
     :content-start nil :contents-end nil
     :max-hits 50 :coding utf-8)))

;;;
;;; Interface functions
;;;

(put 'ndsary :methods 'ndsary-dictionary-methods)
(defun ndsary-dictionary-methods (dictionary)
  (if (or (and (lookup-dictionary-option dictionary :entry-start t)
               (lookup-dictionary-option dictionary :entry-end t))
          (lookup-dictionary-option dictionary :entry-start-end-pairs t))
      '(exact prefix suffix substring keyword)
    '(exact prefix)))

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

(defun ndsary-construct-query-string (query method start end)
  "Costruct search pattern from QUERY string and METHOD.
If START tag is provided, then that will be attached.
If END tag is provided, then that will also be attached."
  (if (and (or (null start) (null end))
           (or (equal method 'suffix)
               (equal method 'substring) 
               (equal method 'keyword)))
      nil
    (concat (if (or (equal method 'exact)
                    (equal method 'prefix))
                start)
            query
            (if (or (equal method 'exact)
                    (equal method 'suffix))
                end))))

(put 'ndsary :search 'ndsary-dictionary-search)
(defun ndsary-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (let ((entry-start (lookup-dictionary-option dictionary :entry-start t))
        (entry-end   (lookup-dictionary-option dictionary :entry-end t))
        (entry-pairs (lookup-dictionary-option dictionary 
                                               :entry-start-end-pairs t))
        (regular     (lookup-dictionary-option dictionary :regular t)))
    (if entry-pairs
        (progn
          (if (functionp entry-pairs)
              (setq entry-pairs (funcall entry-pairs query)))
          (apply 'nconc
                 (mapcar (lambda (x)
                           (ndsary-dictionary-search-each
                            dictionary query (car x) (cdr x) regular))
                         entry-pairs)))
      (ndsary-dictionary-search-each
       dictionary query entry-start entry-end regular))))

(defun ndsary-extract-entries (method string entry-start entry-end regular)
  "Extract entries in accordance with METHOD, STRING, ENTRY-START and ENTRY-END.
If REGULAR is t, then no duplicate start-tag in single line is assumed.
Extraction are done in accordance with follows.
  * exact, keyword:  No need to extract.  Just use query-string.
  * suffix:    <start>.....|[str]</end>
  * substring: <start>...|[str]..</end>
  * prefix:    <start>[str].....</end>.
Entries will be a list of (quote . string)."
  (goto-char (point-min))
  (if (equal method 'exact) (list (cons (concat entry-start string entry-end)
                                        string))
    (if (equal method 'keyword) (list (cons string string))
      (let* ((entry-start-re (regexp-quote entry-start))
             (forward-regexp
              (concat entry-start-re "\\("
                      (if (or (eq method 'suffix) (eq method 'substring)) ".*?")
                      "\\(" (regexp-quote string) "\\)"
                      (if (or (eq method 'prefix) (eq method 'substring)) ".*?")
                      "\\)" (regexp-quote entry-end)))
             start end start1 end1 result)
        (while (re-search-forward forward-regexp nil t)
          (setq end (match-end 0)
                end1 (match-end 1))
          (if (and (null regular)
                   (or (eq method 'suffix) (eq method 'substring)))
              ;; backward double check
              (progn
                (goto-char (match-beginning 2))
                (re-search-backward entry-start-re nil t)
                (setq start (match-beginning 0)
                      start1 (match-end 0)))
            (setq start (match-beginning 0)
                  start1 (match-beginning 1)))
          (setq result (cons (cons (buffer-substring-no-properties start end)
                                   (buffer-substring-no-properties start1 end1))
                             result))
          (goto-char end))
        result))))

(defun ndsary-extract-entries-2 (method string entry-start entry-end)
  "Extract entries in accordance with METHOD, STRING, ENTRY-START and ENTRY-END.
Both ENTRY-START or ENTRY-END can be null.
Extraction are done in accordance with follows.
  * exact:     (start)[str](/end)
  * prefix:    (start)[str].....(/end).
Entries will be a list of (quote . string)."
  (if (not (or (equal method 'exact)
               (equal method 'prefix)))
      (error "Method not supported"))
  (let ((forward-regexp
         (concat (if entry-start (regexp-quote entry-start))
                 "\\(" (regexp-quote string)
                 (if (equal method 'prefix) ".*?")
                 "\\)"
                 (if entry-end (regexp-quote entry-end))))
        result)
    (while (re-search-forward forward-regexp nil t)
      (setq result (cons (cons (buffer-substring-no-properties
                                (match-beginning 0) (match-end 0))
                               (buffer-substring-no-properties
                                (match-beginning 1) (match-end 1)))
                         result)))
    result))
  
(defun ndsary-dictionary-search-each
  (dictionary query entry-start entry-end regular)
  "Return entry list of DICTIONARY QUERY for ENTRY-START and ENTRY-END.
REGULAR is t if dictionary does not have duplicate entries."
  (let* ((method   (lookup-query-method query))
         (string   (lookup-query-string query))
         (dir      (lookup-agent-location
                    (lookup-dictionary-agent dictionary)))
         (file     (expand-file-name
                    (lookup-dictionary-name dictionary) dir))
         (query-string (ndsary-construct-query-string
                        string method entry-start entry-end))
         (max-hits (or (lookup-dictionary-option dictionary :max-hits t)
                       lookup-max-hits))
         (coding (or (lookup-dictionary-option dictionary :coding t)
                     'utf-8))
         (count 0) result)
    (if (null query-string) nil
      (if (/= 0 max-hits)
          ;; count the number of hits
          (with-temp-buffer
            (lookup-with-coding-system coding
              (call-process
               ndsary-sary-program nil t nil "-c" "-i"
               query-string file))
            (goto-char (point-min))
            (if (looking-at "\\([0-9]+\\)")
                (setq count (+ count (string-to-number (match-string 1))))
              0)))
      (cond ((and (/= 0 max-hits) (< max-hits count))
             (list
              (lookup-new-entry
               'regular dictionary "�"
               (format "Error. More than %s hits. (%d)" max-hits count))))
            ((and (= 0 count)) nil) ;; no hit at all.
            ((and (or (null entry-start) (null entry-end))
                  (equal method 'exact))
             ;; no need to search.  Only single entry will be returned.
             (list
              (lookup-new-entry 'regular dictionary string)))
            (t
             ;; extract entries
             (with-temp-buffer
               (lookup-with-coding-system coding
                 (call-process
                  ndsary-sary-program nil t nil "-i"
                  query-string file))
               (setq result
                     (if (and entry-start entry-end)
                         (ndsary-extract-entries
                          method string entry-start entry-end regular)
                       (ndsary-extract-entries-2
                        method string entry-start entry-end)))
               (unless regular
                 (setq result
                       (remove-duplicates
                        result
                        :test (lambda (x y) (equal (car x) (car y))))))
               (mapcar (lambda (x)
                         (lookup-new-entry
                          'regular dictionary
                          (concat (car x)) (cdr x)))
                       result)))))))

(put 'ndsary :content 'ndsary-entry-content)
(defun ndsary-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((string     (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (coding     (or (lookup-dictionary-option dictionary :coding t)
                         'utf-8))
         (dir  (lookup-agent-location
                (lookup-dictionary-agent dictionary)))
         (file (expand-file-name (lookup-dictionary-name dictionary) dir))
         (content-start (lookup-dictionary-option dictionary :content-start t))
         (content-end   (lookup-dictionary-option dictionary :content-end t)))
    (if (equal "�" string) string
      (with-temp-buffer
        (lookup-with-coding-system coding
          (apply 'call-process
                 `(,ndsary-sary-program nil t nil "-i"
                   ,@(if (stringp content-start) (list "-s" content-start)
                       (if (integerp content-start)
                           (list "-A" (number-to-string content-start))
                         (list "-A" "0")))
                   ,@(if (stringp content-end) (list "-e" content-end)
                       (if (integerp content-end)
                           (list "-B" (number-to-string content-end))
                         (list "-B" "0")))
                   ,string ,file)))
        (buffer-string)))))

(provide 'ndsary)

;;; ndsary.el ends here
