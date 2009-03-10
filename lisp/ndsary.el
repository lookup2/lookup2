;;; ndsary.el --- Lookup `sary' interface

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

;; ndsary.el provides the suffix array index searching by using `sary' 
;; program (http://www.namazu.org/sary/).

;; ndsary always support `exact' and `prefix' search, and if `:entry-end'
;; is provided, then `suffix' search is also possible.  

;; If `:content-start' is not provided, then start of the line would be 
;; considered as beginning of the contents.  If `:contents-end' is not 
;; provided, then the end of line will be considered as the end of content.

;;; Usage:
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/wikipedia/jawiki-20090124-abstract.xml"
;;            :dict-spec ((:name "jawiki" :title "Wikipedia 日本語"
;;                         :entry-start "<title>Wikipedia: " 
;;                         :entry-end "</title>"
;;                         :content-start "<doc>" :content-end "</doc>"))
;;            :arranges ((replace remove-xml-tag-entry))
;;            )
;;           ....
;;           ))

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
  '((:name "default" :title "Default"
     :entry-start nil :entry-end nil
     :content-start nil :contents-end nil)))

;;;
;;; Interface functions
;;;

(put 'ndsary :methods 'ndsary-dictionary-methods)
(defun ndsary-dictionary-methods (dictionary)
  '(exact prefix suffix))

(put 'ndsary :list 'ndsary-list)
(defun ndsary-list (agent)
  (if (not (and (executable-find ndsary-sary-program)
                (file-exists-p (lookup-agent-location agent))
                (file-exists-p (concat (lookup-agent-location agent) ".ary"))))
      (progn
        (message "ndsary configuration is incorrect.  dictionary will not be created.") nil)
    (let ((dict-specs (or (lookup-agent-option agent :dict-spec)
                          ndsary-default-dict-specs)))
      (mapcar (lambda (dict-spec) 
                (let* ((dict
                        (lookup-new-dictionary agent (plist-get dict-spec :name)))
                       (id 
                        (lookup-dictionary-id dict)))
                  (apply 
                   'lookup-set-dictionary-options 
                   (cons id dict-spec))
                  dict))
              dict-specs))))

(put 'ndsary :title 'ndsary-title)
(defun ndsary-title (dictionary)
  ;; it has been already initialized at `ndsary-list' phase.
  (lookup-dictionary-option dictionary :title))

(defun ndsary-construct-query-string (query method start end)
  "Costruct search pattern from QUERY string and METHOD.
If START-tag is provided, then that will be attached.
If END-tag is provided, then that will also be attached."
  (if (and (null end) (equal method 'suffix)) nil
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
  (let* ((method   (lookup-query-method query))
         (string   (lookup-query-string query))
         (location (lookup-agent-location
                    (lookup-dictionary-agent dictionary)))
         (entry-start (lookup-dictionary-option dictionary :entry-start))
         (entry-end  (lookup-dictionary-option dictionary :entry-end))
         (query-string (ndsary-construct-query-string
                        string method entry-start entry-end))
         count result)
    (if (null query-string) nil
      ;; proper search
      (if (/= 0 lookup-max-hits)
          (with-temp-buffer
            (lookup-with-coding-system 'utf-8
              (call-process
               ndsary-sary-program nil t nil "-c" "-i"
               query-string (expand-file-name location)))
            (goto-char (point-min))
            (looking-at "\\([0-9]+\\)")
            (setq count (string-to-number (match-string 1)))))
      (cond ((and (/= 0 lookup-max-hits) (< lookup-max-hits count))
             (message "Searching %s for %s exceeds max count." string location) nil)
            ((and (= 0 count))
             (message "Searching %s for %s do not hit." string location) nil)
            (t
             (with-temp-buffer
               (lookup-with-coding-system 'utf-8
                 (call-process
                  ndsary-sary-program nil t nil "-i"
                  query-string (expand-file-name location)))
               (when entry-start
                 (goto-char (point-min))
                 (if (re-search-forward entry-start nil t) (replace-match "")))
               (when entry-end
                 (goto-char (point-max))
                 (if (re-search-backward (concat entry-end ".*\n") nil t)
                     (replace-match "")))
               (if (or (null entry-start) (null entry-end))
                   (let ((start (point-min)))
                     (goto-char start)
                     (while (re-search-forward query-string nil t)
                       (delete-region start (match-beginning 0))
                       (backward-char)
                       (forward-word)
                       (delete-region (point) (line-end-position)))
                     (setq result
                           (split-string (buffer-string) "\n")))
                 (setq result
                       (remove-duplicates ;; current problem ... can not handle multiple data
                        (split-string (buffer-string) (concat entry-end "\\(.\\|\n\\)*?" entry-start))
                        :test 'equal)))
               (mapcar (lambda (x) (lookup-new-entry 'regular dictionary x))
                       result)))))))
                                          
(put 'ndsary :content 'ndsary-entry-content)
(defun ndsary-entry-content (entry)
  "Return string content of ENTRY."
  (let* ((string     (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (location (lookup-agent-location
                    (lookup-dictionary-agent dictionary)))
         (entry-start (lookup-dictionary-option dictionary :entry-start))
         (entry-end  (lookup-dictionary-option dictionary :entry-end))
         (query-string (ndsary-construct-query-string
                        string 'exact entry-start entry-end))
         (content-start (lookup-dictionary-option dictionary :content-start))
         (content-end   (lookup-dictionary-option dictionary :content-end)))
    (with-temp-buffer
      (lookup-with-coding-system 'utf-8
        (apply 'call-process
               `(,ndsary-sary-program nil t nil "-i"
                 ,@(if content-start (list "-s" content-start))
                 ,@(if content-end   (list "-e" content-end))
                 ,query-string ,(expand-file-name location))))
        (buffer-string))))

;; トフティークン事件

;;;
;;; making URL link
;;;

;(widget-convert-button 'link from to :action 'fnction-name :button-keymap nil
;                       :help-echo "click to open browser")
;
;(defun ndsary-press-button (elems el)
;  "When button is pressed, open the browser with specified URL."
;  (goto-char (widget-get elems :from))
;  (let ((data (get-text-property (point)) 'ndsary-url))
;    (when data (browse-url data))))

(provide 'ndsary)

;;; ndsary.el ends here
