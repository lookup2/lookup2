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
;; If `:entry-func' is provided, then the region between
;; `:content-start' and `:entry-end' will be passed to that function
;; (as a form of restricted current buffer with min-point set.)
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
  '(exact prefix suffix substring keyword))

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
  (let ((entry-start (lookup-dictionary-option dictionary :entry-start t))
        (entry-end   (lookup-dictionary-option dictionary :entry-end t))
        (entry-pairs (lookup-dictionary-option dictionary
                                               :entry-start-end-pairs t))
        (entry-func  (lookup-dictionary-option dictionary :entry-func t))
        (content-start (lookup-dictionary-option dictionary :content-start t))
        (string      (lookup-query-string query))
        (method      (lookup-query-method query))
        (regular     (lookup-dictionary-option dictionary :regular t))
        (file        (expand-file-name
                      (lookup-dictionary-name dictionary)
                      (lookup-agent-location
                       (lookup-dictionary-agent dictionary))))
        (coding      (or (lookup-dictionary-option dictionary :coding t)
                         'utf-8))
        (max-hits    (or (lookup-dictionary-option dictionary :max-hits t)
                         lookup-max-hits
                         100))
        entries)
    (if (null entry-pairs)
        (setq entry-pairs (list (cons entry-start entry-end)))
      (if (functionp entry-pairs)
          (setq entry-pairs (funcall entry-pairs query))))
    (setq entries
          (ndsary-file-searches
           file string method entry-pairs
           regular coding max-hits 
           entry-func content-start))
    (mapcar
     (lambda (x) (lookup-new-entry
                  'regular dictionary (car x) (cdr x)))
     entries)))

(put 'ndsary :content 'ndsary-entry-content)
(defun ndsary-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((string         (lookup-entry-code entry))
         (dictionary     (lookup-entry-dictionary entry))
         (coding         (or (lookup-dictionary-option dictionary :coding t)
                             'utf-8))
         (file           (expand-file-name
                          (lookup-dictionary-name dictionary)
                          (lookup-agent-location
                           (lookup-dictionary-agent dictionary))))
         (content-start (lookup-dictionary-option dictionary :content-start t))
         (content-end   (lookup-dictionary-option dictionary :content-end t)))
    (ndsary-file-content file string content-start content-end coding)))

;;;
;;; Main Program
;;;

(defun ndsary-pattern (string method start end)
  "Costruct search pattern from query STRING and METHOD for `sary'.
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
            string
            (if (or (equal method 'exact)
                    (equal method 'suffix))
                end))))

(defun ndsary-regexp-pair (string method start end regular)
  "Construct regexp-pair for filtering the hit from STRING and METHOD.
START and END tag shold also be provided.
Return value should be (FWD-SEARCH . BACK-SEARCH).
If REGULAR is t, then no BACK-SEARCH would be necessary in any case.
BACK-SEARCH may be nil if not necessary (in this case, 1st match
string should be entry.
FWD-SEARCH's 1st match is END-tag, BACK-SEARCH's 1st match is START-tag.
If BACK-SEARCH is nil, FWD-SEARCH's first, and second match
matches START-tag, and END-tag respectively."
;; .....<start>...[string]...<end>......[string?]....
;; A. forward-search-only
;; <string>...[string]...<end> will be searched.
;; B. forward-backward-search
;; (1) search forward for `[string]-<end>'
;; (2) search backward for '<start>-[string]'
  (let ((start-regexp (if (null start) "^\\(\\)"
                      (concat "\\(" (regexp-quote start) "\\)")))
        (end-regexp (if (null end)  "\\(\\)$"
                       (concat "\\(" (regexp-quote end) "\\)")))
        (regexp     (regexp-quote string)))
    ;; forward-search only
    (if (equal method 'exact)
        (cons (concat start-regexp regexp end-regexp) nil)
      (if (equal method 'keyword)
          (cons (concat start-regexp ".+?" end-regexp) nil)
        (if regular
            (if (equal method 'prefix)
                (cons (concat start-regexp regexp ".*?" end-regexp) nil)
              (if (equal method 'postfix)
                  (cons (concat start-regexp ".*?" regexp end-regexp) nil)
                ;; method = 'substring
                (cons (concat start-regexp ".*?" regexp ".*?" end-regexp) nil)))
          ;; forward-backward-search
          (if (equal method 'prefix)
              (setq start-regexp (concat start-regexp regexp))
            (if (equal method 'suffix)
                (setq end-regexp (concat regexp end-regexp))
              ;; method = 'substring
              (setq start-regexp (concat regexp ".*?" end-regexp))))
          (cons end-regexp start-regexp))))))

(defun ndsary-file-searches
  (file string method entry-pairs regular coding max-hits
   &optional entry-func content-start )
  "Return entry list of FILE STRING of METHOD for ENTRY-PAIRS.
For the rest of arguments, please refer `ndsary-file-search'."
  (sort
   (remove-duplicates
    (apply 'nconc
           (mapcar (lambda (x)
                     (ndsary-file-search
                      file string method (car x) (cdr x)
                      regular coding max-hits
                      entry-func content-start))
                   entry-pairs))
    :test (lambda (x y) (equal (car x) (car y))))
   (lambda (x y) (string< (cdr x) (cdr y)))))

(defun ndsary-file-search
  (file string method entry-start entry-end regular coding max-hits
   &optional entry-func content-start )
  "Return entry list of FILE STRING of METHOD for ENTRY-START and ENTRY-END.
CODING specifies file's coding system.
If more than MAX-HITS is hit, then error response query will be returned.
Returned list will be a list of (code . heading).
If ENTRY-FUNC and CONTENT-START is provided, the region between
CONTENT-START to ENTRY-END will be passed to create the ENTRY
value (code heading)."
  (let* ((pattern (ndsary-pattern
                   string method entry-start entry-end))
         (count 0) result)
    (if (null pattern) nil
      (if (/= 0 max-hits)
          (setq count (ndsary-file-match-count file pattern)))
      (cond ((and (/= 0 max-hits) (< max-hits count))
             (list
              (cons "�"
                    (format "Error. More than %s hits. (%d)" max-hits count))))
            ((and (= 0 count)) nil) ;; no hit at all.
            ((and (or (null entry-start) (null entry-end))
                  (equal method 'exact))
             ;; no need to search.  Only single entry will be returned.
             (list
              (cons string string)))
            (t
             ;; execute program.
             (with-temp-buffer
               (lookup-with-coding-system coding
                 (if (and entry-func content-start entry-end)
                     (call-process
                      ndsary-sary-program nil t nil "-i"
                      "-s" content-start "-e" entry-end 
                      pattern file)
                   (call-process
                    ndsary-sary-program nil t nil "-i" pattern file)))
               ;; extract entries
               (setq result
                     (ndsary-extract-entries
                      string method entry-start entry-end regular
                      entry-func content-start))
               ;; remove duplicate entries
               (unless regular
                 (setq result
                       (remove-duplicates
                        result
                        :test (lambda (x y) (equal (car x) (car y))))))
               result))))))

(defun ndsary-file-match-count (file pattern)
  "Return the number of match in FILE for PATTERN."
  (with-temp-buffer
    (lookup-with-coding-system coding
      (call-process
       ndsary-sary-program nil t nil "-c" "-i"
       pattern file))
    (goto-char (point-min))
    (if (looking-at "\\([0-9]+\\)")
        (setq count (+ count (string-to-number (match-string 1))))
      0)))

(defun ndsary-extract-entries
  (string method entry-start entry-end regular
   &optional entry-func content-start)
  "Extract entries in accordance with STRING, METHOD, ENTRY-START and ENTRY-END.
If ID-START and ID-END are specified, they will be used as entry code.
If REGULAR is t, then no duplicate start-tag in single line is assumed.
Extraction are done in accordance with follows.
  * exact, keyword:  No need to extract.  Just use query-string.
  * suffix:    <start>.....|[str]<end>
  * substring: <start>...|[str]..<end>
  * prefix:    <start>[str]......<end>.
Entries will be a list of (code . heading).
If ENTRY-FUNC is provided, it will be called to  extract headr (code . heading).
from the region CONTENT-START and ENTRY-END."
  ;; Additional Note.
  ;; METHOD and STRING is needed to filter out `outside-entry match'.
  ;; Backward Checking is also needed.
  ;; For example, if start-tag='>' and end-tag is '</entry>' and text is
  ;; "<item><entry id="xxx">XXXX</entry>", then
  ;; need to avoid hit "><entry id="xxxx">XXXX</entry>" instead of ">XXXX</entry>".
  (goto-char (point-min))
  (let* ((filter-regexp-pair
          (ndsary-regexp-pair string method entry-start entry-end regular))
         (forward-regexp (car filter-regexp-pair))
         (backward-regexp (cdr filter-regexp-pair))
         word-start word-end code-start code-end
         entry entries)
    (while (re-search-forward forward-regexp nil t)
      (if backward-regexp
          ;; forward-backward-search
          (progn
            (setq word-end (match-beginning 1)
                  code-end (match-end 1))
            (if (re-search-backward backward-regexp
                                 (line-beginning-position) t)
                (setq code-start (match-beginning 1)
                      word-start (match-end 1)
                      entry (cons (buffer-substring-no-properties
                                   code-start code-end)
                                  (buffer-substring-no-properties
                                   word-start word-end))))
            (goto-char code-end))
        ;; forward-search-only
        (setq entry (cons (buffer-substring (match-beginning 1) (match-end 2))
                          (buffer-substring (match-end 1) (match-beginning 2)))))
      ;; entry-func search
      (when (and entry-func content-start)
        (save-excursion
          (save-restriction
            (if (and (progn (search-backward content-start nil t)
                            (setq code-start (match-beginning 0)))
                     (progn (search-forward entry-end nil t)
                            (setq code-end (match-end 0))))
                (progn
                  (narrow-to-region code-start code-end)
                  (goto-char (point-min))
                  (setq entry (funcall entry-func)))
              (setq entry nil)))))
      ;; entry accumulation
      (setq entries (cons entry entries)))
    entries))

(defun ndsary-file-content 
  (file string content-start content-end &optional coding)
  (let ((coding (if (null coding) 'utf-8 coding)))
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
