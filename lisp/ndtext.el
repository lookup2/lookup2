;;; ndtext.el --- Lookup text dictionary search interface -*- lexical-binding: t -*-

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

;; ndtext.el provides grep search facilities for text-based
;; dicionaries.
;;
;; Typical text dictionary looks like as follows:
;;
;; ....
;; <content>
;;  <code>CODE</code>
;;  <head>HEAD</head>
;;  <entry>ENTRY1</entry>
;;  <entry>ENTRY2</entry>
;;  ....
;;  <entry>ENTRY3</entry>
;;  <entry>ENTRY4</entry>
;;  ....
;;  <explanation>....</explanation>
;; </content>
;; ....
;;
;; | option           | value                        | note                                |
;; |------------------+------------------------------+-------------------------------------|
;; | :entry-tags-list | (entry-tags entry-tags...)   |                                     |
;; |                  | function                     | argument: string, method            |
;; |------------------+------------------------------+-------------------------------------|
;; | :entry-tags      | ("<entry"> . "</entry>")     |                                     |
;; |                  | nil                          | default: (nil . "\t")               |
;; |                  | (nil . "</entry>")           | content-start = line beginning      |
;; |                  | function                     | argument: string, method            |
;; |                  |                              | return:   entry-tags                |
;; |------------------+------------------------------+-------------------------------------|
;; | :head-tags       | ("<head>" . "</head>")       |                                     |
;; |                  | nil                          | default: :entry-tags                |
;; |                  | (nil . "</head>")            | head-start = line beginning         |
;; |                  | function                     | argument: content-string            |
;; |                  |                              | return:   head-value                |
;; |------------------+------------------------------+-------------------------------------|
;; | :code-tags       | ("<code>" . "</code>")       |                                     |
;; |                  | nil                          | default: :head-tags                 |
;; |                  | (nil . "</code>")            | code-start = line beginning         |
;; |                  | function                     | argument: string, method, head-tags |
;; |------------------+------------------------------+-------------------------------------|
;; | :content-tags    | ("<content>" . "</content>") | multi-line                          |
;; |                  | nil                          | default: ("\n . "\n")               |
;; |                  | ("\n" . "\n")                | single-line                         |
;; |                  | function (:search)           | argument: string, method, code-tags |
;; |                  | function (:content)          | argument: code, 'code, code-tags    |
;; |------------------+------------------------------+-------------------------------------|
;; | :extension       | ".xml"                       | (not used in ndsary.)               |
;; |                  | nil                          | default: ".txt" is used.            |
;; |------------------+------------------------------+-------------------------------------|
;;

;;
;;; Rationale (why content-tags is dependent of code-tags?)
;;
;; When text dictionary is multi-layered, such as "kanji" and "jukugo", then
;; <content> tag is dependent of <code> (<head>) tags.
;; 
;;; Usage:
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndtext "~/edicts/wikipedia"
;;            :extension ".xml"
;;            :entry-start "<title>Wikipedia: " 
;;            :entry-end "</title>"
;;            :content-start "<doc>" :content-end "</doc>"
;;            :arranges ((replace remove-xml-tag-entry))
;;            )
;;           ....
;;           ))
;;
;; Warning ::
;;
;; 'shift-jis' or 'cp932' for `:encoding' can not be used, as they may
;; contain "[" character in second-byte of multibyte character.

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-lib)
(require 'lookup)



;;;
;;; Customizable variables
;;;

(defgroup ndtext nil
  "Lookup ndwnj interface."
  :group 'lookup-agents)

(defcustom ndtext-grep "xzgrep"
  "*Program name of GNU grep program."
  :type 'string
  :group 'ndtext)

;;;
;;; Internal variables
;;;

;; follwing variables may be customized, even though it's not recommended.
(defvar ndtext-grep-multi-line-options '("-Pzo" "-a"))
(defvar ndtext-grep-single-line-options '("-a"))
(defvar ndtext-grep-max-count-option "--max-count")
(defvar ndtext-cache (make-hash-table :test 'equal))
(defvar ndtext-initialized nil)

;;;
;;; Interface functions
;;;

(put 'ndtext :list 'ndtext-list)
(defun ndtext-list (agent)
  "Return list of dictionaries of AGENT."
  (setq ndtext-cache (make-hash-table :test 'equal))
  (ndtext-initialize)
  (let ((directory (lookup-agent-location agent))
        (extension (or (lookup-agent-option agent :extension)
                       ".txt"))) ; default extension is `.txt'.
    (cond
     ((not (file-directory-p directory))
      (error "ndtext: agent (directory) %s not found." directory))
     (t
      (mapcar (lambda (name)
                (lookup-new-dictionary agent (file-name-nondirectory name)))
              (file-expand-wildcards (concat directory "/*" extension)))))))

(put 'ndtext :methods 'ndtext-dictionary-methods)
(defun ndtext-dictionary-methods (_dictionary)
  '(exact prefix suffix substring)) ;; text))

(put 'ndtext :title 'ndtext-title)
(defun ndtext-title (dictionary)
  "Get title of DICTIONARY."
  (or (lookup-dictionary-option dictionary :title)
      (file-name-sans-extension
       (lookup-dictionary-name dictionary))))

(put 'ndtext :search 'ndtext-dictionary-search)
(defun ndtext-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (ndtext-initialize)
  (ndtext-dictionary-search-common dictionary query 'ndtext))

(put 'ndtext :content 'ndtext-entry-content)
(defun ndtext-entry-content (entry)
  "Return string content of ENTRY."
  (ndtext-initialize)
  (ndtext-entry-content-common entry 'ndtext))

;;
;; common to ndtext/ndbuffer/ndsary
;;

(defun ndtext-dictionary-search-common (dictionary query backend)
  (let* ((dict-id (lookup-dictionary-id dictionary))
         (string  (lookup-query-string query))
         (method  (lookup-query-method query))
         (file    (expand-file-name
                   (lookup-dictionary-name dictionary)
                   (lookup-agent-location
                    (lookup-dictionary-agent dictionary))))
         (coding  (or (lookup-dictionary-option dictionary :coding t)
                      'utf-8)))
    (destructuring-bind 
        (content-tags entry-tags head-tags code-tags entry-tags-list)
        (ndtext-dictionary-options dictionary)
      (if entry-tags (setq entry-tags-list (list entry-tags)))
      (loop for (code head val) in (ndtext-search-multiple
                                    backend file string method
                                    content-tags entry-tags-list
                                    head-tags code-tags coding)
            for entry = (lookup-new-entry 'regular dictionary code head)
            do (puthash (cons dict-id code) val ndtext-cache)
            collect entry))))

(defun ndtext-entry-content-common (entry backend)
  (let* ((dictionary (lookup-entry-dictionary entry))
         (dict-id    (lookup-dictionary-id dictionary))
         (code       (lookup-entry-code entry))
         (coding     (or (lookup-dictionary-option dictionary :coding t)
                         'utf-8)))
    (or (gethash (cons dict-id code) ndtext-cache)
        (destructuring-bind 
            (content-tags entry-tags head-tags code-tags _entry-tags-list)
            (ndtext-dictionary-options dictionary)
          (ndtext-process backend 'content dict-id code 'code
                          content-tags entry-tags head-tags
                          code-tags coding)))))

(defun ndtext-dictionary-options (dictionary)
  "Obtain needed options from DICTIONARY and STRING."
  (let ((content-tags    (lookup-dictionary-option dictionary :content-tags t))
        (entry-tags      (lookup-dictionary-option dictionary :entry-tags t))
        (head-tags       (lookup-dictionary-option dictionary :head-tags t))
        (code-tags       (lookup-dictionary-option dictionary :code-tags t))
        (entry-tags-list (lookup-dictionary-option dictionary :entry-tags-list t)))
    (list content-tags entry-tags head-tags code-tags entry-tags-list)))

;;;
;;; Main Program
;;;

(defun ndtext-initialize ()
  (unless ndtext-initialized
    (let ((grep (if (equal (file-name-nondirectory ndtext-grep) "xzgrep")
                   (or (getenv "GREP")
                       (executable-find "grep"))
                 ndtext-grep)))
      (cond
       ((not (executable-find ndtext-grep))
        (error "ndtext: %s is not found." ndtext-grep) nil)
       ((not (string-match 
              "(GNU grep)"
              (with-temp-buffer
                (call-process grep nil t nil "-V")
                (buffer-string))))
        (error "ndtext: `grep' is not GNU grep.") nil)))
    (setq ndtext-initialized t)))

;; |                       | search                  | get                      |
;; |-----------------------+-------------------------+--------------------------|
;; | max-count-option      | maxhits=lookup-max-hits | maxhits=1                |
;; | grep-pattern          | entry                   | <code>code</code>        |
;; | single-line-options   | -a                      | -a                       |
;; | multiple-line-options | -Pzo -a                 | -Pzo -a                  |
;; | single-line-regexp    | (..<entry>)##</entry>.. | (..)<code>##</code>..    |
;; | multiple-line-regexp  | <c>(..<e>)..</e>..</c>  | <c>(..)<co>..</co>..</c> |
;; | obtaining content     | ndtext-collect-results  | buffer-string            |

;; handle perl exception of `+' and `\n'.
(defun ndtext-regexp-quote (string)
  (when string
    (replace-regexp-in-string "\\\\\\+" "+"
      (replace-regexp-in-string "\n" "\\\\n" (regexp-quote string)))))

(put 'ndtext :options         'ndtext-options)
(put 'ndtext :pattern         'ndtext-pattern)
(put 'ndtext :program-symbol  'ndtext-grep)
(put 'ndtext :max-count-check nil)

;;;
;;; ndtext/ndbuffer/ndsary common search interface
;;;

(defun ndtext-normalize-options (string method content-tags entry-tags head-tags code-tags)
  (typecase entry-tags
    (null     (setq entry-tags '(nil . "\t")))
    (function (setq entry-tags (funcall entry-tags string method))))
  (typecase head-tags
    (null     (setq head-tags entry-tags))
    )
  (typecase code-tags
    (null     (if (functionp head-tags)
                  (error "If head-tags is function, code-tags must not be null!")
                (setq code-tags head-tags)))
    (function (setq code-tags (funcall code-tags string method head-tags))))
  (typecase content-tags
    (null     (setq content-tags '("\n" . "\n")))
    (function (setq content-tags (funcall content-tags string method code-tags))))
  (list content-tags entry-tags head-tags code-tags))

(defun ndtext-search-multiple
  (agent file string method &optional content-tags entry-tags-list
   head-tags code-tags coding)
  (if (functionp entry-tags-list)
      (setq entry-tags-list (funcall entry-tags-list string method)))
  (cl-remove-duplicates
   (loop for entry-tags in entry-tags-list
         nconc (ndtext-process agent 'search file string
                               method content-tags
                               entry-tags head-tags
                               code-tags coding))
   :test (lambda (x y) (string= (car x) (car y)))))

(declare-function ndbuffer-process "ndbuffer")
(defun ndtext-process
  (agent action file string method &optional content-tags entry-tags
   head-tags code-tags coding)
  ;; AGENT ::= ndtext/ndbuffer/ndsary
  ;; ACTION ::= search  (string=string, method=exact/prefix/suffix/text)
  ;;          | content (string=code,   method=code)
  (destructuring-bind
      (content-tags entry-tags head-tags code-tags)
      (ndtext-normalize-options string method content-tags entry-tags head-tags code-tags)
    (if (equal agent 'ndbuffer)
        ;; ndbuffer
        (ndbuffer-process action file
                          string method content-tags entry-tags head-tags code-tags)
      ;; ndsary/ndtext
      (let* ((single-line (equal content-tags '("\n" . "\n")))
             (options (funcall (get agent :options) action single-line content-tags))
             (pattern (funcall (get agent :pattern)
                               string (if (eq action 'search) method 'exact) content-tags 
                               (if (eq action 'search) entry-tags code-tags) single-line))
             (max-count-check (get agent :max-count-check))
             (program (eval (get agent :program-symbol)))
             (arguments (append options (list pattern (file-truename file))))
             status)
        (if (or (equal action 'content)
                (and (equal action 'search)
                     (or (null max-count-check)
                         (funcall max-count-check file pattern coding))))
            (with-temp-buffer
              ;; This newline insertion will make single-line search matches to first line.
              (if single-line (insert "\n"))
              (lookup-debug-message "ndtext:program=%s, args=%s" program arguments)
              (lookup-with-coding-system coding
                (setq status (apply 'call-process program nil t nil arguments)))
              (if (< 1 status)
                  (let ((content (buffer-string)))
                    (error "ndtext: %s returned %s.  buffer=%s" program status content)))
              (if (equal action 'content) (buffer-string)
                (ndtext-collect-results content-tags code-tags head-tags single-line))))))))

;;; common to ndtext/ndsary

(defun ndtext-collect-results (content-tags code-tags head-tags single-line)
  "Collect results from buffer."
  (let* (results start end code head content
         (code-start (car code-tags))
         (code-end   (regexp-quote (cdr code-tags)))
         ;; normalize codes
         (code-start (if code-start (regexp-quote code-start) "^"))
         )
    (lookup-debug-message "ndtext:buffer=%s" (buffer-string))
    (goto-char (point-min))
    (while (re-search-forward (car content-tags) nil t)
      (setq start (match-beginning 0))
      (if (re-search-forward (if single-line "$" (car content-tags)) nil t)
          (setq end (match-beginning 0))
        (setq end (point-max)))
      (setq content (buffer-substring start end))
      (lookup-debug-message "ndtext:content=%s" content)
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        ;; code
        (re-search-forward (concat code-start ".+?" code-end) nil t)
        (setq code (match-string 0))
        ;; head (if only code exists)
        (when code
          (if (functionp head-tags)
              (setq head (funcall head-tags (buffer-substring (point-min) (point-max))))
            (let* ((head-start (car head-tags))
                   (head-start (if head-start (regexp-quote head-start) "^"))
                   (head-end (regexp-quote (cdr head-tags))))
              (goto-char (point-min))
              (re-search-forward (concat head-start "\\(.+?\\)" head-end) nil t)
              (setq head (match-string 1))))))
      (goto-char end)
      (lookup-debug-message "code=%s, head=%s" code head)
      (if (and code head)
          (push (list code head content) results)))
    (nreverse results)))


;;; `ndtext' specific functions

(defun ndtext-options (action single-line _content-tags)
  `(,ndtext-grep-max-count-option 
    ,(if (eq action 'content) "1" (format "%d" lookup-max-hits))
    ,@(if single-line ndtext-grep-single-line-options ndtext-grep-multi-line-options)))

(defun ndtext-pattern (string method content-tags tags single-line)
  (if single-line (ndtext-single-line-pattern string method tags)
    (ndtext-multi-line-pattern string method content-tags tags)))

(defun ndtext-single-line-pattern (string method tags)
  (let ((start-tag (car tags))
        (end-tag (cdr tags)))
    (case method
      ('exact (concat (if (and start-tag (not (string= start-tag "\n")))
                          (ndtext-regexp-quote start-tag) "^")
                      string
                      (if (string= end-tag "\n") "$"
                        (ndtext-regexp-quote end-tag))))
      ('prefix (concat (if (and start-tag (not (string= start-tag "\n")))
                          (ndtext-regexp-quote start-tag) "^")
                       string))
      ('suffix (concat string
                       (if (string= end-tag "\n") "$"
                         (ndtext-regexp-quote end-tag))))
      (t string))))

(defun ndtext-multi-line-pattern (string method content-tags tags)
  "Construct grep pattern."
  (let ((tag-start   (ndtext-regexp-quote (car tags)))
        (tag-end     (ndtext-regexp-quote (cdr tags)))
        (content-start (ndtext-regexp-quote (car content-tags)))
        (any-char (concat "(?:(?!"
                          (ndtext-regexp-quote (cdr content-tags))
                          ").)*")))
    (concat
     "(?s)"             ; PCRE_DOTALL
     content-start      ; <content> 
     (if tag-start
         ;; * <tag> is defined
         ;;   <content>..?(search-word)..(</content>)
         ;;              exact     :: <tag>string</tag>
         ;;              prefix    :: <tag>string
         ;;              suffix    :: string</tag>
         ;;              substring :: string (<tag>..string..</tag>)
         ;;              text      :: string
         (concat any-char "?"
                 (case method
                   ('exact (concat tag-start string tag-end))
                   ('prefix (concat tag-start string))
                   ('suffix (concat string tag-end))
                   (t string))) ;; text/code
       ;; * <tag> is not defined
       ;;   <content>(search-word)..(</content>)
       ;;              exact     :: string</tag>
       ;;              prefix    :: string
       ;;              suffix    :: ..?string</tag>
       ;;              substring :: ..?string
       ;;              text      :: ..?</tag>..string
       ;;    ※ ".." = (!</content)*
       (case method
         ('exact (concat string tag-end))
         ('prefix string)
         ('suffix (concat any-char "?" tag-end))
         (t (concat any-char "?" string)))) ;; text/code
     any-char)))

(provide 'ndtext)

;;; ndtext.el ends here
