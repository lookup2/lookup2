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

(require 'lookup-utils)
(require 'lookup-vars)

;;;;;;;;;;;;;;;;;;;;
;; Search Method
;;;;;;;;;;;;;;;;;;;;

(defconst lookup-search-methods
  '(exact prefix suffix substring wildcard regexp keyword text))

(defconst lookup-all-search-methods
  (nconc '(default index menu reference) lookup-search-methods))

(defconst lookup-search-method-marks
  '((exact . ?=) (prefix . ?>) (suffix . ?<) (substring . ?-) (wildcard . ?*)
    (regexp . ?%) (keyword . ?@) (text . ?/) (index . ?I) (menu . ?M)))


;;;;;;;;;;;;;;;;;;;;
;; Search Query
;;;;;;;;;;;;;;;;;;;;

(defstruct lookup-query method string pattern)

(defun lookup-new-query (method string &optional pattern)
  (unless pattern
    (setq pattern (cond ((eq method 'prefix) (concat string "*"))
			((eq method 'suffix) (concat "*" string))
			((eq method 'substring) (concat "*" string "*"))
			((eq method 'reference) (concat "%:" string))
			(t string))))
  (make-lookup-query :method method :string string :pattern pattern))

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
    (if (string= string "")
	(error "Invalid query pattern"))
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

(defstruct lookup-module name dictionaries bookmarks priority-alist)

(defun lookup-new-module (name &rest dicts)
  (let ((module (make-lookup-module :name name)) dict prio)
    (if (eq (car dicts) t)
	(setq dicts (mapcar 'lookup-dictionary-id lookup-dictionary-list)))
    (setq dicts
	  (mapcar (lambda (spec)
		    (if (stringp spec)
			(setq dict spec spec nil)
		      (setq dict (car spec) spec (cdr spec)))
		    (setq dict (lookup-get-dictionary dict))
		    (setq prio (if (memq :priority spec)
				   (plist-get spec :priority)
				 (or (lookup-dictionary-ref dict :priority)
				     t)))
		    (lookup-module-dictionary-set-priority module dict prio)
		    dict)
		  dicts))
    (setf (lookup-module-dictionaries module) dicts)
    (if lookup-cache-file (lookup-restore-module-attributes module))
    module))

(defun lookup-module-add-bookmark (module entry)
  (let ((bookmarks (delq entry (lookup-module-bookmarks module))))
    (setf (lookup-module-bookmarks module) (cons entry bookmarks))))

(defun lookup-module-remove-bookmark (module entry)
  (let ((bookmarks (delq entry (lookup-module-bookmarks module))))
    (setf (lookup-module-bookmarks module) bookmarks)))

(defun lookup-module-dictionary-priority (module dictionary)
  (lookup-assq-get (lookup-module-priority-alist module) dictionary))

(defsetf lookup-module-dictionary-priority lookup-module-dictionary-set-priority)
(defun lookup-module-dictionary-set-priority (module dictionary value)
  (let ((alist (lookup-module-priority-alist module)))
    (setq alist (lookup-assq-put alist dictionary value))
    (setf (lookup-module-priority-alist module) alist)))


;;;;;;;;;;;;;;;;;;;;
;; Search Agent
;;;;;;;;;;;;;;;;;;;;

(defstruct lookup-agent class location options id)

(defun lookup-new-agent (class &optional location &rest options)
  (let* ((agent (make-lookup-agent :class class :location location
				   :id (concat (symbol-name class)
					       ":" location)))
	 (opts (lookup-assoc-get lookup-agent-option-alist
				 (lookup-agent-id agent))))
    (while options
      (plist-put opts (caar options) (cdar options) )
      (setq options (cdr options)))
    (setf (lookup-agent-options agent) opts)
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
  (or (lookup-get-property agent 'dictionaries)
      (let ((id (lookup-agent-id agent)) dicts)
	(message "Setting up %s..." id)
	(setq dicts (lookup-agent-command agent :list))
	(message "Setting up %s...done" id)
	(lookup-put-property agent 'dictionaries dicts)
	dicts)))

(defun lookup-agent-clear (agent)
  (mapc 'lookup-dictionary-clear (lookup-agent-dictionaries agent))
  (lookup-agent-command agent :kill))


;;;;;;;;;;;;;;;;;;;;
;; Dictionary
;;;;;;;;;;;;;;;;;;;;

(defstruct lookup-dictionary agent name options id)

(defun lookup-new-dictionary (agent name)
  (make-lookup-dictionary :agent agent :name name
			  :id (concat (lookup-agent-id agent) "/" name)))

(defun lookup-dictionary-initialize (dictionary)
  (if lookup-cache-file (lookup-restore-dictionary-attributes dictionary))
  (let* ((id (lookup-dictionary-id dictionary))
	 (plist (lookup-assoc-get lookup-dictionary-option-alist id)))
    ;; Load support file
    (let ((file (lookup-assoc-get lookup-support-alist id)))
      (when file
	(let ((lookup-support-agent (lookup-agent-class
				     (lookup-dictionary-agent dictionary)))
	      (lookup-support-options nil))
	  (load file)
	  (let ((list lookup-support-options))
	    (while list
	      (setq plist (plist-put plist (car list) (cadr list)))
	      (setq list (cddr list)))))))
    (setf (lookup-dictionary-options dictionary) plist)))

(defun lookup-dictionary-ref (dictionary key)
  (lookup-agent-ref (lookup-dictionary-agent dictionary) key))

(defun lookup-dictionary-command (dictionary command &rest args)
  (let ((func (lookup-dictionary-ref dictionary command)))
    (if (functionp func) (apply func dictionary args) func)))

(defun lookup-dictionary-clear (dictionary)
  (lookup-dictionary-command dictionary :clear))

;; options

(put 'lookup-dictionary-get lisp-indent-function 2)
(defun lookup-dictionary-get (dictionary key default)
  (or (lookup-get-property dictionary key)
      ;; Initialize
      (unless (lookup-get-property dictionary 'initialized)
	(lookup-dictionary-initialize dictionary)
	(lookup-put-property dictionary 'initialized t)
	(lookup-get-property dictionary key))
      ;; Get default value
      (let ((val (funcall default)))
	(lookup-put-property dictionary key val)
	val)))

(defun lookup-dictionary-option (dictionary key &optional inherit)
  (or (plist-get (lookup-dictionary-options dictionary) key)
      (if inherit
	  (lookup-agent-option (lookup-dictionary-agent dictionary) key))))

(defun lookup-dictionary-title (dictionary)
  (lookup-dictionary-get dictionary 'title
    (lambda () (or (lookup-dictionary-option dictionary :title)
		   (lookup-dictionary-command dictionary :title)
		   (lookup-dictionary-name dictionary)))))

(defun lookup-dictionary-heading (dictionary)
  (lookup-dictionary-get dictionary 'heading
    (lambda () (truncate-string-to-width (lookup-dictionary-title dictionary)
					 lookup-title-width nil ? ))))

(defun lookup-dictionary-transformer (dictionary)
  (lookup-dictionary-get dictionary 'transformer
    (lambda () (lookup-dictionary-option dictionary :transformer t))))

(defun lookup-dictionary-default-method (dictionary)
  (lookup-dictionary-get dictionary 'default-method
    (lambda () (or (lookup-dictionary-transformer dictionary)
		   (lookup-dictionary-option dictionary :default-method)
		   lookup-default-method))))

(defun lookup-dictionary-methods (dictionary)
  (lookup-dictionary-get dictionary 'methods
    (lambda () (lookup-dictionary-command dictionary :methods))))

(defun lookup-dictionary-arranges (dictionary)
  (lookup-dictionary-get dictionary 'arrange-table
    (lambda ()
      (let* ((table1 (lookup-dictionary-option dictionary :arrange-table))
	     (table2 (lookup-dictionary-ref dictionary :arrange-table)))
	(mapcar (lambda (pair)
		  (or (lookup-assq-get table1 (car pair))
		      (lookup-assq-get table2 (car pair))
		      (cdr pair)))
		lookup-arrange-table)))))

(defun lookup-dictionary-gaiji-table (dictionary)
  (lookup-dictionary-get dictionary 'gaiji-table
    (lambda () (or (lookup-dictionary-option dictionary :gaiji-table)
		   (lookup-make-gaiji-table)))))

(defun lookup-dictionary-gaiji-regexp (dictionary)
  (lookup-dictionary-get dictionary 'gaiji-regexp
    (lambda () (lookup-dictionary-option dictionary :gaiji-regexp t))))

(defun lookup-dictionary-reference-pattern (dictionary)
  (lookup-dictionary-get dictionary 'reference-pattern
    (lambda () (lookup-dictionary-option dictionary :reference-pattern t))))

;; Search commands

(defun lookup-dictionary-menu (dictionary)
  (let ((menu (lookup-dictionary-get dictionary 'menu
		(lambda () (or (lookup-dictionary-command dictionary :menu)
			       'no-exists)))))
    (unless (eq menu 'no-exists) menu)))

(defun lookup-dictionary-gaiji (dictionary code)
  (let* ((table (lookup-dictionary-gaiji-table dictionary))
	 (gaiji (lookup-gaiji-table-ref table code)))
    (cond
     ((lookup-gaiji-p gaiji) gaiji)
     ((eq gaiji 'no-gaiji) nil)
     (t
      (let ((spec gaiji) glyph)
	(unless spec
	  (setq spec (lookup-dictionary-command dictionary :gaiji code)))
	(when (vectorp spec)
	  (setq spec (lookup-gaiji-concrete spec)))
	(if (not spec)
	    (setq gaiji 'no-gaiji)
	  (if (stringp spec)
	      (setq gaiji (lookup-new-gaiji spec))
	    (setq glyph (or (car spec)
			    (lookup-dictionary-command dictionary :font code)))
	    (setq gaiji (lookup-new-gaiji glyph (cadr spec)))))
	(lookup-gaiji-table-set table code gaiji)
	(unless (eq gaiji 'no-gaiji) gaiji))))))

(defun lookup-dictionary-search (dictionary query)
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query)))
    ;; Default method may vary, depending on dictionaries
    (when (eq method 'default)
      (setq method (lookup-dictionary-default-method dictionary))
      (setq query (lookup-new-query method string)))
    (let* ((search (if (memq method lookup-search-methods)
		       'lookup-regular-search
		     method))
	   (entries (lookup-dictionary-search-internal
		     dictionary query search)))
      (run-hook-with-args 'lookup-after-dictionary-search-hook
			  dictionary entries)
      entries)))

(defun lookup-dictionary-search-internal (dictionary query search)
  (let (entries)
    (unless lookup-force-update
      (setq entries (lookup-dictionary-search-cache-get dictionary query)))
    (unless entries
      (setq entries (or (funcall search dictionary query) 'no-exists))
      (lookup-dictionary-search-cache-put dictionary query entries))
    (unless (eq entries 'no-exists)
      entries)))

(defun lookup-regular-search (dictionary query)
  (lookup-dictionary-command dictionary :search query))

(defun lookup-stemming-search (dictionary query)
  (let* ((string (downcase (lookup-query-string query)))
	 (query (lookup-new-query 'exact string))
	 entries dict heading last-entry)
    (if (setq entries (reverse (lookup-dictionary-search-internal
				dictionary query 'lookup-regular-search)))
	(setq last-entry (lookup-entry-substance (car entries))))
    (dolist (string (cdr (nreverse (stem-english string))))
      (when (> (length string) 3)
	(setf (lookup-query-string query) string)
	(dolist (entry (lookup-dictionary-search-internal
			dictionary query 'lookup-regular-search))
	  ;; Sometimes stemming creates duplicate entries with different
	  ;; headings.  Let's remove them.
	  (unless (eq (lookup-entry-substance entry) last-entry)
	    (setq entries (cons (lookup-new-slink entry) entries))
	    (setq last-entry (lookup-entry-substance entry))))))
    (nreverse entries)))

(defconst lookup-obarray (make-vector 1511 nil))

(defun lookup-dictionary-search-cache-get (dictionary query)
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query))
	(cache (lookup-get-property dictionary 'entry-cache)))
    (lookup-multi-get 'cache method (intern string lookup-obarray))))

(defun lookup-dictionary-search-cache-put (dictionary query entries)
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query))
	(cache (lookup-get-property dictionary 'entry-cache)))
    (lookup-multi-put 'cache method (intern string lookup-obarray) entries)
    (lookup-put-property dictionary 'entry-cache cache)))


;;;;;;;;;;;;;;;;;;;;
;; Entry
;;;;;;;;;;;;;;;;;;;;

(defstruct lookup-entry type dictionary code bookmark id)

(defun lookup-new-entry (type dictionary code &optional heading)
  (let (entry)
    (cond
     ((eq type 'regular)
      (if (setq entry (lookup-get-entry
		       (concat (lookup-dictionary-id dictionary) "#" code)))
	  (setq type 'link code entry)))
     ((or (eq type 'link) (eq type 'slink))
      (setq code (lookup-entry-substance code))))
    (let ((id (apply 'concat (lookup-dictionary-id dictionary)
		     (cond ((eq type 'regular) (list "#" code))
			   ((eq type 'dynamic) (list "?" code))
			   (t (list "->" (lookup-entry-code code)))))))
      (setq entry (make-lookup-entry :type type :dictionary dictionary
				     :code code :id id)))
    (when (eq type 'regular)
      (if lookup-cache-file (lookup-restore-entry-attributes entry))
      (lookup-put-entry entry))
    (if heading (setf (lookup-entry-heading entry) heading))
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
  (let ((func (or (lookup-get-property entry command)
		  (lookup-entry-ref entry command))))
    (if (functionp func) (apply func entry args) func)))

(defsetf lookup-entry-heading lookup-entry-set-heading)
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
    (lookup-put-property entry 'heading heading)))

(defun lookup-entry-heading (entry)
  (or (lookup-get-property entry 'heading)
      (let ((heading (or (progn (setq entry (lookup-entry-substance entry))
				(lookup-entry-command entry :heading))
			 (lookup-entry-code entry))))
	(setf (lookup-entry-heading entry) heading)
	(lookup-get-property entry 'heading))))

(defun lookup-entry-heading-insert (entry)
  (let ((start (point))
	(end (progn (insert (lookup-entry-heading entry)) (point))))
    (add-text-properties start end (list 'mouse-face 'highlight
					 'lookup-entry entry))
    (lookup-map-over-property start end 'lookup-gaiji
			      'lookup-gaiji-glyph-paste)))

(defun lookup-entry-content (entry)
  (lookup-put-property (lookup-entry-substance entry) 'refered t)
  (lookup-entry-command entry :content))

(defun lookup-entry-references (entry)
  (or (lookup-get-property entry 'references)
      (let ((references (lookup-entry-command entry :dynamic)))
	(lookup-put-property entry 'references references)
	(lookup-put-property entry 'refered t)
	references)))

(defun lookup-entry-refered-p (entry)
  (lookup-get-property (lookup-entry-substance entry) 'refered))

(defun lookup-entry-open (entry)
  (lookup-entry-command entry :open))


;;;;;;;;;;;;;;;;;;;;
;; Session
;;;;;;;;;;;;;;;;;;;;

(defstruct lookup-session module query entries dictionaries excursion)

(defun lookup-new-session (module query entries)
  (make-lookup-session :module module :query query :entries entries))

(defun lookup-session-read-only-p (session)
  (eq (lookup-query-method (lookup-session-query session)) 'reference))

(defun lookup-session-display (session)
  (if lookup-last-session
      (setf (lookup-session-excursion lookup-last-session)
	    (lookup-search-excursion)))
  (lookup-summary-display session)
  (setq lookup-current-session session
	lookup-last-session session))

(defun lookup-session-open (session)
  (setq lookup-current-session session)
  (lookup-session-display session)
  (lookup-history-push lookup-search-history session))


;;;;;;;;;;;;;;;;;;;;
;; History
;;;;;;;;;;;;;;;;;;;;

(defstruct lookup-history stack position)

(defun lookup-new-history ()
  (make-lookup-history :position 0))

(defun lookup-history-length (history)
  (length (lookup-history-stack history)))

(defun lookup-history-ref (history &optional n)
  (let ((stack (lookup-history-stack history)))
    (setq n (or n (lookup-history-position history)))
    (nth (- (if (< n 0) -1 (length stack)) n) stack)))

(defun lookup-history-push (history obj)
  (let ((stack (lookup-history-stack history))
	(position (lookup-history-position history)))
    (setq stack (nthcdr (- (length stack) position) stack))
    (unless (eq obj (car stack))
      (setf (lookup-history-stack history) (cons obj stack))
      (if (and (> lookup-max-history 0) (>= position lookup-max-history))
	  (setcdr (nthcdr (- lookup-max-history 2) stack) nil)
	(setf (lookup-history-position history) (1+ position))))))

(defun lookup-history-move (history n)
  (let ((length (lookup-history-length history))
	(position (lookup-history-position history)))
    (cond
     ((= length 0) (error "No session in the history"))
     ((and (= position 1) (< n 0)) (error "This is the first session"))
     ((and (= position length) (> n 0)) (error "This is the last session")))
    (setq position (+ position n))
    (setq position (if (< position 1) 1
		     (if (> position length) length
		       position)))
    (setf (lookup-history-position history) position)
    (lookup-history-ref history)))


;;;;;;;;;;;;;;;;;;;;
;; Gaiji
;;;;;;;;;;;;;;;;;;;;

(defstruct lookup-gaiji glyph alter)

(defun lookup-new-gaiji (glyph &optional alter)
  (unless (stringp glyph)
    (setq glyph (lookup-gaiji-glyph-compose glyph))
    (setq alter (or alter lookup-gaiji-alternative)))
  (make-lookup-gaiji :glyph glyph :alter alter))

(defun lookup-gaiji-insert (gaiji)
  (let ((glyph (lookup-gaiji-glyph gaiji))
	(alter (lookup-gaiji-alter gaiji))
	(start (point)))
    (insert (or alter glyph))
    (if alter (put-text-property start (point) 'lookup-gaiji gaiji))))

;; gaiji glyph

(cond
 ((featurep 'xemacs)
  (defun lookup-gaiji-glyph-compose (spec)
    (cond
     ((stringp spec)
      (make-glyph (vector 'string :data spec)))
     ((eq (aref spec 0) 'compose)
      (make-glyph (vector 'string :data (aref spec 1))))
     ((eq (aref spec 0) 'xbm)
      (make-glyph (vector 'xbm :data (lookup-decode-graph spec))))
     (t (error "Invalid glyph spec: %S" spec))))

  (defun lookup-gaiji-glyph-paste (start end glyph)
    (set-extent-property (extent-at start nil 'lookup-gaiji) 'invisible t)
    (let (extent extents)
      (while (setq extent (extent-at start nil nil extent 'at))
	(if (eq (extent-start-position extent) (extent-end-position extent))
	    (setq extents (cons extent extents))))
      (while extents
	(set-extent-endpoints (car extents) end end)
	(setq extents (cdr extents)))
      (set-extent-begin-glyph (make-extent end end) glyph))))
 (t
  (defun lookup-gaiji-glyph-compose (spec)
    (cond
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
	(list 'image :type 'xbm :ascent 'center
	      :width width :height height :data data)))
     (t (error "Invalid glyph spec: %S" spec))))

  (defun lookup-gaiji-glyph-paste (start end glyph)
    (add-text-properties start end
			 (list 'display glyph
			       'intangible glyph
			       'rear-nonsticky (list 'display))))))

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

(provide 'lookup-types)

;;; lookup-types.el ends here
