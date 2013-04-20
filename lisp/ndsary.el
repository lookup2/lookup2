;;; ndsary.el --- Lookup `sary' interface -*- lexical-binding: t -*-

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
;; 
;; options are the same as the ones in `ndtext.el'.
;; (except :extension is unnecessary.)
;;
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
;;            :content-tags '("<doc>" . "</doc>")
;;            :entry-tags ("<title>Wikipedia: " . "</title>")
;;            :arranges ((replace remove-xml-tag-entry))
;;            )
;;           ....
;;           ))
;;
;;  If you want to give multiple search methods to single XML file, 
;;  then please make a hard-link copy of them and create purpose-specific
;;  ".ary" files individually.

;;; Code:

(eval-when-compile (require 'cl))
(require 'lookup)
(require 'ndtext)



;;;
;;; Internal variables
;;;

(defvar ndsary-sary "sary")

;;;
;;; Interface functions
;;;

(put 'ndsary :list 'ndsary-list)
(defun ndsary-list (agent)
  "Return list of dictionaries of AGENT."
  (setq ndtext-cache (make-hash-table :test 'equal))
  (let ((directory (lookup-agent-location agent)))
    (cond
     ((not (executable-find ndsary-sary))
      (error "ndsary: program not found.") nil)
     ((not (file-exists-p (lookup-agent-location agent)))
      (error "ndsary: agent %s not found." 
             (lookup-agent-location agent)) nil)
     (t
      (mapcar (lambda (name)
                (lookup-new-dictionary agent (file-name-nondirectory 
                                              (file-name-sans-extension name))))
              (file-expand-wildcards (concat directory "/*" ".ary")))))))

(put 'ndsary :methods 'ndsary-dictionary-methods)
(defun ndsary-dictionary-methods (_dictionary)
  `(exact prefix suffix substring))

(put 'ndsary :title 'ndsary-title)
(defun ndsary-title (dictionary)
  "Get title of DICTIONARY."
  (or (lookup-dictionary-option dictionary :title)
      (let ((name (lookup-dictionary-name dictionary)))
        (file-name-sans-extension name))))

(put 'ndsary :search 'ndsary-dictionary-search)
(defun ndsary-dictionary-search (dictionary query)
  "Return entry list of DICTIONARY for QUERY."
  (ndtext-dictionary-search-common dictionary query 'ndsary))

(put 'ndsary :content 'ndsary-entry-content)
(defun ndsary-entry-content (entry)
  "Return string content of ENTRY."
  (ndtext-entry-content-common entry 'ndsary))

;;;
;;; Main Program
;;;

(put 'ndsary :options         'ndsary-options)
(put 'ndsary :pattern         'ndsary-pattern)
(put 'ndsary :program-symbol  'ndsary-sary)
(put 'ndsary :max-count-check 'ndsary-max-count-check)

(defun ndsary-max-count-check (file sary-pattern coding)
  "Return nil if there are too many hits."
  (let (num status)
    (with-temp-buffer
      (lookup-with-coding-system (or coding 'utf-8)
        (setq status (apply 'call-process ndsary-sary nil t nil 
                            (list "-c" sary-pattern (file-truename file)))))
      (setq num (string-to-number (buffer-string))))
    (if (< 0 status) (error "ndsary: %s rerurns %s. buffer=%s" ndsary-sary status (buffer-string)))
    (when (or (< num lookup-max-hits)
              (y-or-n-p 
               (format "ndsary: %s are hit.  Display them all?" num))) t)))

(defun ndsary-options (_action single-line content-tags)
  (unless single-line (list "-s" (car content-tags) "-e" (cdr content-tags))))

(defun ndsary-pattern (string method _content-tags tags _single-line)
  "Costruct search pattern from query STRING and METHOD for `sary'.
If START tag is provided, then that will be attached.
If END tag is provided, then that will also be attached."
    (concat (if (or (equal method 'exact)
                    (equal method 'prefix))
                (or (car tags) "\n"))
            string
            (if (or (equal method 'exact)
                    (equal method 'suffix))
                (cdr tags))))

(provide 'ndsary)

;;; ndsary.el ends here
