;;; lookup-types.el --- internal data types
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

(require 'evi)
(require 'lookup-utils)
(require 'lookup-vars)

(put 'lookup-defstruct 'lisp-indent-function 2)
(defmacro lookup-defstruct (name slots &rest args)
  (let* ((str (symbol-name name)) (n 0)
	 (prefix (concat "lookup-" str "-"))
	 (tag (list 'quote (intern str)))
	 (id-format (eval (plist-get args ':id-format)))
	 (properties (eval (plist-get args ':with-properties)))
	 ;; function names
	 (f-make (intern (concat "lookup-make-" str)))
	 (f-p (intern (concat prefix "p")))
	 (f-get-prop (intern (concat prefix "get-property")))
	 (f-put-prop (intern (concat prefix "put-property"))))
    (nconc
     (list
      'progn
      ;; (defun lookup-make-NAME (SLOT...)
      ;;   (let* ((id ID) (symbol (make-symbol (concat "#<NAME:" id ">"))))
      ;;     (set symbol (vector NAME id SLOT...))
      ;;     symbol))
      (let ((format (list 'concat (concat "#<" str " ") 'id ">")))
	(list 'defun f-make slots
	      (list 'let* (list (list 'id (or id-format '(lookup-new-id)))
				(list 'symbol (list 'make-symbol format)))
		    (list 'set 'symbol
			  (cons 'vector (cons tag (cons 'id slots))))
		    'symbol)))
      ;; (defun lookup-NAME-p (NAME)
      ;;   (and (symbolp NAME) (boundp NAME) (setq NAME (symbol-value NAME))
      ;;        (vectorp NAME) (eq (aref NAME 0) :NAME)))
      (list 'defun f-p (list name)
	    (list 'and (list 'symbolp name) (list 'boundp name)
		  (list 'setq name (list 'symbol-value name))
		  (list 'vectorp name) (list 'eq (list 'aref name 0) tag))))
     (apply 'nconc
	    (mapcar (lambda (slot)
		      (let* ((str (symbol-name slot))
			     (f-ref (intern (concat prefix str)))
			     (f-set (intern (concat prefix "set-" str))))
			(list
			 ;; (defun lookup-NAME-SLOT (NAME)
			 ;;   (aref (symbol-value NAME) n))
			 (list 'defun f-ref (list name)
			       (list 'aref (list 'symbol-value name)
				     (setq n (1+ n))))
			 ;; (defun lookup-NAME-set-SLOT (NAME SLOT)
			 ;;   (aset (symbol-value NAME) n SLOT))
			 (list 'defun f-set (list name slot)
			       (list 'aset (list 'symbol-value name)
				     n slot)))))
		    (cons 'id slots)))
     (list
      ;; (defalias 'lookup-NAME-get-property 'get)
      (list 'defalias (list 'quote f-get-prop) ''get)
      ;; (defalias 'lookup-NAME-put-property 'put)
      (list 'defalias (list 'quote f-put-prop) ''put)
      ;; (defalias 'lookup-NAME-setplist 'setplist)
      (list 'defalias (list 'quote (intern (concat prefix "setplist")))
	    ''setplist))
     (apply 'nconc
	    (mapcar (lambda (prop)
		      (let* ((str (symbol-name prop))
			     (ref (intern (concat prefix str)))
			     (set (intern (concat prefix "set-" str))))
			(list
			 ;; (defsubst lookup-NAME-PROP (NAME)
			 ;;   (lookup-NAME-get-property NAME 'PROP))
			 (list 'defsubst ref (list name)
			       (list f-get-prop name (list 'quote prop)))
			 ;;(defsubst lookup-NAME-set-PROP (NAME PROP)
			 ;; (lookup-NAME-put-property NAME 'PROP PROP))
			 (list 'defsubst set (list name prop)
			       (list f-put-prop name
				     (list 'quote prop) prop)))))
		    properties)))))

(defvar lookup-new-id 1000)
(defun lookup-new-id ()
  (number-to-string (setq lookup-new-id (1+ lookup-new-id))))


;;;;;;;;;;;;;;;;;;;;
;; Search Method
;;;;;;;;;;;;;;;;;;;;

(defconst lookup-search-methods
  '(exact prefix suffix substring wildcard regexp keyword text))

(defconst lookup-all-search-methods
  (nconc '(default transform index menu reference) lookup-search-methods))

(defconst lookup-search-method-marks
  '((exact . ?=) (prefix . ?>) (suffix . ?<) (substring . ?-) (wildcard . ?*)
    (regexp . ?%) (keyword . ?@) (text . ?/) (index . ?I) (menu . ?M)))


;;;;;;;;;;;;;;;;;;;;
;; Search Query
;;;;;;;;;;;;;;;;;;;;

(lookup-defstruct query (method string pattern)
  :id-format '(or pattern string))

(defun lookup-new-query (method string &optional pattern)
  (let ((query (lookup-make-query method string pattern)))
    (unless pattern
      (setq pattern (cond ((eq method 'prefix) (concat string "*"))
			  ((eq method 'suffix) (concat "*" string))
			  ((eq method 'substring) (concat "*" string "*"))
			  ((eq method 'reference) (concat "%:" string))
			  (t string)))
      (lookup-query-set-pattern query pattern))
    query))

(defun lookup-parse-pattern (pattern)
  (let (method string)
    (cond
     ;; 'word' -> match exactly
     ((string-match "^'\\(.*\\)'$" pattern)
      (setq method 'exact string (match-string 1 pattern)))
     ;; /word/ -> match regexp
     ((string-match "^/\\(.*\\)/$" pattern)
      (setq method 'regexp string (match-string 1 pattern)))
     ;; /word  -> search text
     ((string-match "^/" pattern)
      (setq method 'text string (substring pattern 1)))
     ;; @word  -> match keyword
     ((string-match "^@" pattern)
      (setq method 'keyword string (substring pattern 1)))
     ;; *word* -> match substring
     ((string-match "^\\*\\([^*?]*\\)\\*$" pattern)
      (setq method 'substring string (match-string 1 pattern)))
     ;; word*  -> match prefixes
     ((string-match "^\\([^*?]*\\)\\*$" pattern)
      (setq method 'prefix string (match-string 1 pattern)))
     ;; *word  -> match suffixes
     ((string-match "^\\*\\([^*?]*\\)$" pattern)
      (setq method 'suffix string (substring pattern 1)))
     ;; w*o?d  -> match wildcard
     ((string-match "[*?]" pattern)
      (setq method 'wildcard string pattern))
     ;; default
     (t (setq method 'default string pattern)))
    (lookup-new-query method string pattern)))

(defun lookup-query-to-regexp (query)
  (let* ((method (lookup-query-method query))
	 (string (lookup-query-string query))
	 (quote (regexp-quote string)))
    (cond ((eq method 'exact) (concat "^" quote "$"))
	  ((eq method 'prefix) (concat "^" quote))
	  ((eq method 'suffix) (concat quote "$"))
	  ((eq method 'substring) quote)
	  ((eq method 'regexp) string)
	  ((eq method 'text) string)
	  ((eq method 'keyword) (concat "\\<" quote "\\>"))
	  ((eq method 'wildcard)
	   (setq string (if (string-match "^\\*" string)
			    (substring string 1)
			  (concat "^" string)))
	   (setq string (if (string-match "\\*$" string)
			    (substring string 0 (match-beginning 0))
			  (concat string "$")))
	   (let ((start 0))
	     (while (string-match "*" string start)
	       (setq string (replace-match ".*" t t string)
		     start (+ (match-end 0) 1))))
	   (while (string-match "?" string)
	     (setq string (replace-match "." t t string)))
	   string)
	  (t (error "Invalid search method for regexp: %s" method)))))

(defun lookup-query-to-wildcard (query)
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query)))
    (cond ((eq method 'exact) string)
	  ((eq method 'prefix) (concat string "*"))
	  ((eq method 'suffix) (concat "*" string))
	  ((eq method 'substring) (concat "*" string "*"))
	  ((eq method 'wildcard) string)
	  (t (error "Invalid search method for wildcard: %s" method)))))


;;;;;;;;;;;;;;;;;;;;
;; Search Module
;;;;;;;;;;;;;;;;;;;;

(lookup-defstruct module (name)
  :id-format 'name
  :with-properties '(dictionaries bookmarks priority-alist))

(defun lookup-new-module (name &rest dicts)
  (let ((module (lookup-make-module name)) dict prio)
    (if (eq (car dicts) t)
	(setq dicts (mapcar 'car (lookup-dictionary-alist))))
    (setq dicts
	  (mapcar (lambda (spec)
		    (if (stringp spec)
			(setq dict spec spec nil)
		      (setq dict (car spec) spec (cdr spec)))
		    (setq dict (lookup-get-dictionary dict))
		    (setq prio (if (memq ':priority spec)
				   (plist-get spec ':priority)
				 (or (lookup-dictionary-ref dict ':priority)
				     t)))
		    (lookup-module-dictionary-set-priority module dict prio)
		    dict)
		  dicts))
    (lookup-module-set-dictionaries module dicts)
    (if lookup-cache-file (lookup-restore-module-attributes module))
    module))

(defun lookup-module-add-bookmark (module entry)
  (let ((bookmarks (delq entry (lookup-module-bookmarks module))))
    (lookup-module-set-bookmarks module (cons entry bookmarks))))

(defun lookup-module-remove-bookmark (module entry)
  (let ((bookmarks (delq entry (lookup-module-bookmarks module))))
    (lookup-module-set-bookmarks module bookmarks)))

(defun lookup-module-dictionary-priority (module dictionary)
  (lookup-assq-get (lookup-module-priority-alist module) dictionary))

(defun lookup-module-dictionary-set-priority (module dictionary value)
  (let ((alist (lookup-module-priority-alist module)))
    (setq alist (lookup-assq-put alist dictionary value))
    (lookup-module-set-priority-alist module alist)))


;;;;;;;;;;;;;;;;;;;;
;; Search Agent
;;;;;;;;;;;;;;;;;;;;

(lookup-defstruct agent (class location)
  :id-format '(concat (symbol-name class) ":" location)
  :with-properties '(options))

(defun lookup-new-agent (class &optional location &rest options)
  (let* ((agent (lookup-make-agent class location))
	 (opts (lookup-assoc-get lookup-agent-option-alist
				 (lookup-agent-id agent))))
    (while options
      (plist-put opts (caar options) (cdar options) )
      (setq options (cdr options)))
    (lookup-agent-set-options agent opts)
    (if lookup-cache-file (lookup-restore-agent-attributes agent))
    agent))

(defun lookup-agent-ref (agent key)
  (require (lookup-agent-class agent))
  (get (lookup-agent-class agent) key))

(defun lookup-agent-option (agent key)
  (or (plist-get (lookup-agent-options agent) key)
      (lookup-agent-ref agent key)))

(defun lookup-agent-command (agent command &rest args)
  (let ((func (lookup-agent-ref agent command)))
    (if (functionp func) (apply func agent args) func)))

(defun lookup-agent-dictionaries (agent)
  (or (lookup-agent-get-property agent 'dictionaries)
      (let ((id (lookup-agent-id agent)) dicts)
	(message "Setting up %s..." id)
	(setq dicts (lookup-agent-command agent ':list))
	(message "Setting up %s...done" id)
	(lookup-agent-put-property agent 'dictionaries dicts)
	dicts)))

(defun lookup-agent-kill (agent)
  (lookup-agent-command agent ':kill)
  (setq lookup-agent-alist (delq (rassq agent lookup-agent-alist)
				 lookup-agent-alist)))


;;;;;;;;;;;;;;;;;;;;
;; Dictionary
;;;;;;;;;;;;;;;;;;;;

(lookup-defstruct dictionary (agent name)
  :id-format '(concat (lookup-agent-id agent) "/" name)
  :with-properties '(options))

(defun lookup-new-dictionary (agent name)
  (let ((dict (lookup-make-dictionary agent name)))
    (if lookup-cache-file (lookup-restore-dictionary-attributes dict))
    dict))

;; options

(defun lookup-dictionary-ref (dictionary key)
  (lookup-agent-ref (lookup-dictionary-agent dictionary) key))

(defun lookup-dictionary-complement (dictionary)
  (let ((opts (lookup-assoc-get lookup-dictionary-option-alist
				(lookup-dictionary-id dictionary))))
    (lookup-dictionary-set-options dictionary opts))
  (let ((file (lookup-assoc-get lookup-complement-alist
				(lookup-dictionary-id dictionary)))
	(lookup-complement-agent (lookup-agent-class
				  (lookup-dictionary-agent dictionary)))
	lookup-complement-options)
    (when file
      (if lookup-complement-directory
	  (setq file (expand-file-name file lookup-complement-directory)))
      (load file)
      (let ((plist (lookup-dictionary-options dictionary))
	    (list lookup-complement-options))
	(while list
	  (setq plist (plist-put plist (car list) (cadr list)))
	  (setq list (cddr list)))
	(lookup-dictionary-set-options dictionary plist)))))

(defun lookup-dictionary-option (dictionary key &optional inherit)
  (or (plist-get (lookup-dictionary-options dictionary) key)
      (unless (lookup-dictionary-get-property dictionary 'complemented)
	(lookup-dictionary-complement dictionary)
	(lookup-dictionary-put-property dictionary 'complemented t)
	(plist-get (lookup-dictionary-options dictionary) key))
      (if inherit
	  (lookup-agent-option (lookup-dictionary-agent dictionary) key))))

(defun lookup-dictionary-title (dictionary)
  (or (lookup-dictionary-get-property dictionary 'title)
      (let ((title (or (lookup-dictionary-option dictionary ':title)
		       (lookup-dictionary-command dictionary ':title)
		       (lookup-dictionary-name dictionary))))
	(lookup-dictionary-put-property dictionary 'title title)
	title)))

(defun lookup-dictionary-head (dictionary)
  (or (lookup-dictionary-get-property dictionary 'head)
      (let* ((title (lookup-dictionary-title dictionary))
	     (head (truncate-string-to-width title lookup-head-width nil ? )))
	(lookup-dictionary-put-property dictionary 'head head)
	head)))

(defun lookup-dictionary-transformer (dictionary)
  (or (lookup-dictionary-get-property dictionary 'transformer)
      (let ((trans (lookup-dictionary-option dictionary ':transformer t)))
	(lookup-dictionary-put-property dictionary 'transformer trans)
	trans)))

(defun lookup-dictionary-default-method (dictionary)
  (or (lookup-dictionary-get-property dictionary 'default-method)
      (let ((dm (or (if (lookup-dictionary-transformer dictionary) 'transform)
		    (lookup-dictionary-option dictionary ':default-method)
		    lookup-default-method)))
	(lookup-dictionary-put-property dictionary 'default-method dm)
	dm)))

(defun lookup-dictionary-methods (dictionary)
  (or (lookup-dictionary-get-property dictionary 'methods)
      (let ((methods (lookup-dictionary-command dictionary ':methods)))
	(lookup-dictionary-put-property dictionary 'methods methods)
	methods)))

(defun lookup-dictionary-arranges (dictionary)
  (or (lookup-dictionary-get-property dictionary 'arranges)
      (let* ((table1 (lookup-dictionary-option dictionary ':arrange-table))
	     (table2 (lookup-dictionary-ref dictionary ':arrange-table))
	     (arranges (mapcar (lambda (pair)
				 (or (lookup-assq-get table1 (car pair))
				     (lookup-assq-get table2 (car pair))
				     (cdr pair)))
			       lookup-arrange-table)))
	(lookup-dictionary-put-property dictionary 'arranges arranges)
	arranges)))

(defun lookup-dictionary-gaiji-table (dictionary)
  (or (lookup-dictionary-get-property dictionary 'gaiji-table)
      (let ((table (lookup-dictionary-option dictionary ':gaiji-table)))
	(setq table (or table (lookup-make-gaiji-table)))
	(lookup-dictionary-put-property dictionary 'gaiji-table table)
	table)))

(defun lookup-dictionary-gaiji-regexp (dictionary)
  (or (lookup-dictionary-get-property dictionary 'gaiji-regexp)
      (let ((regexp (lookup-dictionary-option dictionary ':gaiji-regexp t)))
	(lookup-dictionary-put-property dictionary 'gaiji-regexp regexp)
	regexp)))

(defun lookup-dictionary-reference-pattern (dictionary)
  (or (lookup-dictionary-get-property dictionary 'reference-pattern)
      (let ((ptrn (lookup-dictionary-option dictionary ':reference-pattern t)))
	(lookup-dictionary-put-property dictionary 'reference-pattern ptrn)
	ptrn)))

;; commands

(defun lookup-dictionary-command (dictionary command &rest args)
  (let ((func (lookup-dictionary-ref dictionary command)))
    (if (functionp func) (apply func dictionary args) func)))

(defun lookup-dictionary-menu (dictionary)
  (let ((menu (lookup-dictionary-get-property dictionary 'menu)))
    (unless menu
      (setq menu (or (lookup-dictionary-command dictionary ':menu) 'no-exists))
      (lookup-dictionary-put-property dictionary 'menu menu))
    (unless (eq menu 'no-exists) menu)))

(defun lookup-dictionary-search (dictionary query)
  "Search DICTIONARY for QUERY.
If `lookup-force-update' is non-nil, then this function ignores
internal caches."
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query))
	entries)
    (when (eq method 'default)
      (setq method (lookup-dictionary-default-method dictionary))
      (setq query (lookup-new-query method string)))
    (unless lookup-force-update
      (setq entries
	    (lookup-dictionary-search-cache-get dictionary method string)))
    (unless entries
      (setq entries (if (eq method 'transform)
			;; To avoid duplicate records
			(let ((lookup-enable-record nil))
			  (funcall (lookup-dictionary-transformer dictionary)
				   dictionary query))
		      (or (lookup-dictionary-command dictionary ':search query)
			  'no-exists)))
      (lookup-dictionary-search-cache-put dictionary method string entries))
    (if lookup-enable-record (lookup-record-dictionary-used dictionary))
    (unless (eq entries 'no-exists)
      (if lookup-enable-record
	  (lookup-foreach 'lookup-record-entry-found entries))
      entries)))

(defun lookup-dictionary-search-cache-get (dictionary method string)
  (let ((cache (lookup-dictionary-get-property dictionary 'entries-cache)))
    (lookup-multi-get 'cache method (lookup-intern-string string))))

(defun lookup-dictionary-search-cache-put (dictionary method string entries)
  (let ((cache (lookup-dictionary-get-property dictionary 'entries-cache)))
    (lookup-multi-put 'cache method (lookup-intern-string string) entries)
    (lookup-dictionary-put-property dictionary 'entries-cache cache)))

(defun lookup-dictionary-gaiji (dictionary code)
  (let* ((table (lookup-dictionary-gaiji-table dictionary))
	 (gaiji (lookup-gaiji-table-ref table code)))
    (cond
     ((lookup-gaiji-p gaiji) gaiji)
     ((eq gaiji 'no-gaiji) nil)
     (t
      (let ((spec gaiji) glyph)
	(unless spec
	  (setq spec (lookup-dictionary-command dictionary ':gaiji code)))
	(when (vectorp spec)
	  (setq spec (lookup-gaiji-concrete spec)))
	(if (not spec)
	    (setq gaiji 'no-gaiji)
	  (if (stringp spec)
	      (setq gaiji (lookup-new-gaiji spec))
	    (setq glyph (or (car spec)
			    (lookup-dictionary-command dictionary ':font code)))
	    (setq gaiji (lookup-new-gaiji glyph (cadr spec)))
	    (when (and lookup-enable-record
		       (vectorp glyph)
		       (eq (aref glyph 0) 'xbm))
	      (lookup-record-gaiji-loaded gaiji))))
	(lookup-gaiji-table-set table code gaiji)
	(if (not (eq gaiji 'no-gaiji)) gaiji))))))

(defun lookup-dictionary-kill (dictionary)
  (lookup-dictionary-command dictionary ':clear))


;;;;;;;;;;;;;;;;;;;;
;; Entry
;;;;;;;;;;;;;;;;;;;;

;; There are 4 types of entry:
;;
;; `regular' - for entries found by search
;;               CODE: entry code by string
;; `link'    - for hard links to another entry
;;               CODE: the linked entry
;; `slink'   - for soft links to another entry
;;               CODE: the linked entry
;; `dynamic' - for dynamic references
;;               CODE: reference code by string

(lookup-defstruct entry (type dictionary code)
  :id-format '(apply 'concat (lookup-dictionary-id dictionary)
		     (cond ((eq type 'regular) (list "#" code))
			   ((eq type 'dynamic) (list "?" code))
			   (t (list "->" (lookup-entry-code code)))))
  :with-properties '(bookmark))

(defun lookup-new-entry (type dictionary code &optional heading)
  (let (entry)
    (cond
     ((eq type 'regular)
      (if (setq entry (lookup-get-entry
		       (concat (lookup-dictionary-id dictionary) "#" code)))
	  (setq type 'link code entry)))
     ((or (eq type 'link) (eq type 'slink))
      (setq code (lookup-entry-substance code))))
    (setq entry (lookup-make-entry type dictionary code))
    (when (eq type 'regular)
      (if lookup-cache-file (lookup-restore-entry-attributes entry))
      (lookup-put-entry entry))
    (if heading (lookup-entry-set-heading entry heading))
    entry))

(defun lookup-entry-substance (entry)
  (let ((type (lookup-entry-type entry)))
    (if (or (eq type 'link) (eq type 'slink))
	(lookup-entry-code entry)
      entry)))

(defun lookup-new-slink (entry)
  (setq entry (lookup-entry-substance entry))
  (lookup-new-entry 'slink (lookup-entry-dictionary entry)
		    entry (lookup-entry-heading entry)))

(defun lookup-entry-ref (entry key)
  (lookup-dictionary-ref (lookup-entry-dictionary entry) key))

;; commands

(defun lookup-entry-command (entry command &rest args)
  (let ((func (or (lookup-entry-get-property entry command)
		  (lookup-entry-ref entry command))))
    (if (functionp func) (apply func entry args) func)))

(defun lookup-entry-set-heading (entry heading)
  (let ((type (lookup-entry-type entry))
	(regexp (lookup-dictionary-gaiji-regexp
		 (lookup-entry-dictionary entry))))
    (if (and regexp (string-match regexp heading))
	(with-temp-buffer
	  (insert heading)
	  (goto-char (point-min))
	  (lookup-arrange-gaijis entry)
	  (setq heading (buffer-string))))
    (cond ((eq type 'slink) (setq heading (concat "-> " heading)))
	  ((eq type 'dynamic) (setq heading (concat "-> [" heading "]"))))
    (lookup-entry-put-property entry 'heading heading)))

(defun lookup-entry-heading (entry)
  (or (lookup-entry-get-property entry 'heading)
      (let ((heading (or (progn (setq entry (lookup-entry-substance entry))
				(lookup-entry-command entry ':heading))
			 (lookup-entry-code entry))))
	(lookup-entry-set-heading entry heading)
	(lookup-entry-get-property entry 'heading))))

(defun lookup-entry-heading-insert (entry)
  (let ((start (point))
	(end (progn (insert (lookup-entry-heading entry)) (point))))
    (add-text-properties start end (list 'mouse-face 'highlight
					 'lookup-entry entry))
    (lookup-map-over-property start end 'lookup-gaiji
			      'lookup-gaiji-glyph-paste)))

(defun lookup-entry-content (entry)
  (lookup-entry-put-property (lookup-entry-substance entry) 'refered t)
  (lookup-entry-command entry ':content))

(defun lookup-entry-references (entry)
  (or (lookup-entry-get-property entry 'references)
      (let ((references (lookup-entry-command entry ':dynamic)))
	(lookup-entry-put-property entry 'references references)
	(lookup-entry-put-property entry 'refered t)
	references)))

(defun lookup-entry-refered-p (entry)
  (lookup-entry-get-property (lookup-entry-substance entry) 'refered))

(defun lookup-entry-open (entry)
  (lookup-entry-command entry ':open))


;;;;;;;;;;;;;;;;;;;;
;; Session
;;;;;;;;;;;;;;;;;;;;

(lookup-defstruct session (type module)
  :with-properties '(query dictionaries entries excursion))

(defalias 'lookup-new-session 'lookup-make-session)

(defun lookup-session-ref (session key)
  (get (lookup-session-type session) key))

(defun lookup-session-display (session)
  (if lookup-last-session
      (lookup-session-save-excursion lookup-last-session))
  (funcall (lookup-session-ref session 'display) session)
  (setq lookup-current-session session
	lookup-last-session session))

(defun lookup-session-save-excursion (session)
  (let ((func (lookup-session-ref session 'excursion)))
    (when func
      (lookup-session-set-excursion session (funcall func)))))

(defun lookup-compare-session (s1 s2)
  (and (eq (lookup-session-type s1) (lookup-session-type s2))
       (eq (lookup-session-module s1) (lookup-session-module s2))
       (cond
	((eq (lookup-session-type s1) 'lookup-select-session) t)
	((eq (lookup-session-type s1) 'lookup-search-session)
	 (let ((q1 (lookup-session-query s1)) (q2 (lookup-session-query s2)))
	   (and (eq (lookup-query-method q1) (lookup-query-method q2))
		(string= (lookup-query-string q1) (lookup-query-string q2))
		(equal (lookup-session-dictionaries s1)
		       (lookup-session-dictionaries s2))))))))


;;;;;;;;;;;;;;;;;;;;
;; History
;;;;;;;;;;;;;;;;;;;;

(lookup-defstruct history (stack position))

(defun lookup-new-history ()
  (lookup-make-history nil 0))

(defun lookup-history-length (history)
  "Return the length of HISTORY.
Return 0 if HISTORY has never been used."
  (length (lookup-history-stack history)))

(defun lookup-history-ref (history &optional n)
  "Return the N-th object in HISOTORY."
  (let ((stack (lookup-history-stack history)))
    (setq n (or n (lookup-history-position history)))
    (nth (- (if (< n 0) -1 (length stack)) n) stack)))

(defun lookup-history-push (history object)
  (let ((stack (lookup-history-stack history))
	(position (lookup-history-position history)))
    (setq stack (nthcdr (- (length stack) position) stack))
    (unless (eq object (car stack))
      (lookup-history-set-stack history (cons object stack))
      (if (and (> lookup-max-history 0) (>= position lookup-max-history))
	  (setcdr (nthcdr (- lookup-max-history 2) stack) nil)
	(lookup-history-set-position history (1+ position))))))


;;;;;;;;;;;;;;;;;;;;
;; Gaiji
;;;;;;;;;;;;;;;;;;;;

(lookup-defstruct gaiji (glyph alter))

(defun lookup-new-gaiji (glyph &optional alter)
  (unless (stringp glyph)
    (setq glyph (lookup-gaiji-glyph-compose glyph))
    (setq alter (or alter lookup-gaiji-alternate)))
  (lookup-make-gaiji glyph alter))

(defun lookup-gaiji-insert (gaiji)
  (let ((glyph (lookup-gaiji-glyph gaiji))
	(alter (lookup-gaiji-alter gaiji))
	(start (point)))
    (insert (or alter glyph))
    (if alter (put-text-property start (point) 'lookup-gaiji gaiji))
    (if lookup-enable-record (lookup-record-gaiji-inserted gaiji))))

;; gaiji glyph

(defun lookup-gaiji-glyph-possible-p ()
  (and lookup-enable-gaiji window-system lookup-gaiji-compose-function))

(defun lookup-gaiji-glyph-compose (xbm)
  (funcall lookup-gaiji-compose-function xbm))

(defun lookup-gaiji-glyph-paste (start end glyph)
  (funcall lookup-gaiji-paste-function start end glyph))

(cond
 ((featurep 'xemacs)
  (defun lookup-glyph-compose (spec)
    (cond
     ((stringp spec)
      (make-glyph (vector 'string ':data spec)))
     ((eq (aref spec 0) 'compose)
      (make-glyph (vector 'string ':data (aref spec 1))))
     ((eq (aref spec 0) 'xbm)
      (let (width height data)
	(with-temp-buffer
	  (insert (aref spec 1))
	  (goto-char (point-min))
	  (if (re-search-forward "width[ \t]+\\([0-9]+\\)")
	      (setq width (string-to-int (match-string 1))))
	  (if (re-search-forward "height[ \t]+\\([0-9]+\\)")
	      (setq height (string-to-int (match-string 1))))
	  (while (re-search-forward "0x\\(..\\)" nil t)
	    (setq data (cons (string-to-int (match-string 1) 16) data)))
	  (setq data (concat (nreverse data))))
	(make-glyph (vector 'xbm :data (list width height data)))))
     (t (error "Invalid glyph spec: %S" spec))))

  (defun lookup-glyph-paste (start end glyph)
    (set-extent-property (extent-at start nil 'lookup-gaiji) 'invisible t)
    (let (extent extents)
      (while (setq extent (extent-at start nil nil extent 'at))
	(if (eq (extent-start-position extent) (extent-end-position extent))
	    (setq extents (cons extent extents))))
      (while extents
	(set-extent-endpoints (car extents) end end)
	(setq extents (cdr extents)))
      (set-extent-begin-glyph (make-extent end end) glyph)))
  )
 ((featurep 'mule)
  (defun lookup-bitmap-compose (spec)
    (cond
     ((stringp spec) spec)
     ((eq (aref spec 0) 'compose)
      (apply 'compose-chars (string-to-list (aref spec 1))))
     ((eq (aref spec 0) 'xbm)
      (with-temp-buffer
	(require 'bitmap)
	(insert (aref spec 1))
	(let ((cmp (bitmap-decode-xbm
		    (bitmap-read-xbm-buffer (current-buffer)))))
	  (bitmap-compose (aref cmp 0)))))
     (t (error "Invalid glyph spec: %S" spec))))

  (defun lookup-bitmap-paste (start end glyph)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'after-string glyph)))
  ))

;; gaiji table

(defun lookup-make-gaiji-table ()
  (make-vector 377 0))

(defsubst lookup-gaiji-table-set (table code gaiji)
  (set (intern code table) gaiji))

;; Use defsubst because this function had better be fast.
(defsubst lookup-gaiji-table-ref (table code)
  (let ((symbol (intern code table)))
    (if (boundp symbol) (symbol-value symbol))))

(defun lookup-new-gaiji-table (spec)
  (let ((table (lookup-make-gaiji-table)))
    (while spec
      (lookup-gaiji-table-set table (caar spec) (cdar spec))
      (setq spec (cdr spec)))
    table))


;;;;;;;;;;;;;;;;;;;;
;; Object Management
;;;;;;;;;;;;;;;;;;;;

(defun lookup-module-alist ()
  "Return an alist of all modules defined in `lookup-search-modules'.
If `lookup-search-modules' is nil, this function creates a module
named \"default\" with all available dictionaries."
  (or lookup-module-alist
      (setq lookup-module-alist
	    (mapcar (lambda (spec)
		      (let ((module (apply 'lookup-new-module spec)))
			(cons (lookup-module-name module) module)))
		    (or lookup-search-modules '(("default" t)))))))

(defun lookup-get-module (name)
  "Return the search module named NAME.
If there is no buffer named NAME, return nil.
NAME may also be a module; if so, the value is that module."
  (cond ((lookup-module-p name) name)
	((stringp name) (lookup-assoc-get (lookup-module-alist) name))
	(t (signal 'wrong-type-argument (list 'stringp name)))))

(defun lookup-agent-alist ()
  "Return a list of all agents defined in `lookup-search-agents'."
  (or lookup-agent-alist
      (setq lookup-agent-alist
	    (mapcar (lambda (spec)
		      (let ((agent (apply 'lookup-new-agent spec)))
			(cons (lookup-agent-id agent) agent)))
		    (or lookup-search-agents
			(setq lookup-search-agents '((ndtut))))))))

(defun lookup-get-agent (id)
  "Return the search agent specified ID.
If there is no agent specified ID, return nil.
ID may also be a agent; if so, the value is that agent."
  (cond ((lookup-agent-p id) id)
	((stringp id) (lookup-assoc-get (lookup-agent-alist) id))
	(t (signal 'wrong-type-argument (list 'stringp id)))))

(defun lookup-dictionary-alist ()
  "Return a list of all available dictionaries.
If the variable `lookup-search-dictionaries' is defined, this function
generates dictionaries according to it.  If not, this tries to collect
all dictionaries from all available agents."
  (or lookup-dictionary-alist
      (setq lookup-dictionary-alist
	    (apply 'append
		   (mapcar (lambda (pair)
			     (mapcar (lambda (dict)
				       (cons (lookup-dictionary-id dict) dict))
				     (lookup-agent-dictionaries (cdr pair))))
			   (lookup-agent-alist))))))

(defun lookup-get-dictionary (id)
  "Return the dictionary specified ID.
If there is no dictionary specified ID, return nil.
ID may also be a dictionary; if so, the value is that dictionary."
  (cond ((lookup-dictionary-p id) id)
	((stringp id) (lookup-assoc-get (lookup-dictionary-alist) id))
	(t (signal 'wrong-type-argument (list 'stringp id)))))

(defun lookup-entry-list ()
  "Return a list of the all entries created by now.
This function never creates any new entries."
  (let (entries)
    (when lookup-entry-table
      (mapatoms (lambda (symbol)
		  (setq entries (cons (symbol-value symbol) entries)))
		lookup-entry-table))
    entries))

(defun lookup-put-entry (entry)
  (if (not lookup-entry-table)
      (setq lookup-entry-table (make-vector 377 0)))
  (set (intern (lookup-entry-id entry) lookup-entry-table) entry))

(defun lookup-get-entry (id)
  "Return the entry specified ID.
If there is no entry specified ID, return nil.
ID may also be an entry; if so, the value is that entry."
  (cond ((lookup-entry-p id) id)
	((stringp id)
	 (when lookup-entry-table
	   (let ((symbol (intern id lookup-entry-table)))
	     (if (boundp symbol) (symbol-value symbol)))))
	(t (signal 'wrong-type-argument (list 'stringp id)))))

(defun lookup-get-entry-create (id)
  (or (lookup-get-entry id)
      (when (string-match "#" id)
	(let ((dict (substring id 0 (match-beginning 0)))
	      (code (substring id (match-end 0))))
	  (lookup-new-entry 'regular (lookup-get-dictionary dict) code)))))

(defun lookup-gaiji-list ()
  (let (gaijis)
    (lookup-foreach
     (lambda (pair)
       (mapatoms (lambda (code)
		   (setq code (symbol-value code))
		   (if (lookup-gaiji-p code)
		       (setq gaijis (cons code gaijis))))
		 (lookup-dictionary-gaiji-table (cdr pair))))
     (lookup-dictionary-alist))
    gaijis))

(provide 'lookup-types)

;;; lookup-types.el ends here
