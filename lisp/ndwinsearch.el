;;; ndwinsearch.el --- search agent for `Windows Search'
;; Copyright (C) 2009 Taichi KAWABATA <kawabata.taichi@gmail.com>

;; Author: Taichi KAWABATA <kawabata.taichi@gmail.com>
;; Keywords: Microsoft Windows, Windows Search, search

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

;;; Documentation

;; This agent will search for the file that contains specified
;; keywords by using `Microsoft Windows Search'.
;;
;; You need to install `wdsgrep.exe' (ver 4.1 or later) to use this
;; agent.  `wdsgrep.exe' can be downloaded from the following site:
;;
;; http://ext.noue.org/src/
;;
;;
;; Example:
;; 
;; (setq lookup-search-agents
;;       '(....
;;         (ndwinsearch "C:/Documents and Settings/") ;; Search for specified directory
;;         (ndwinsearch "D:/" :ext ".log")             ;; Search for specified directory with extension ".log".
;;        ))

;;; Code:

;;;
;;; Customizable variables
;;;

(defgroup ndwinsearch nil
  "Lookup csrd interface."
  :group 'lookup-search-agents)

(defcustom ndwinsearch-search-program "D:\\wdsgrep-4.1test\\wdsgrep.exe"
  "*Command name and location of `wdsgrep.exe'."
  :type 'string
  :group 'ndwinsearch)

(defcustom ndwinsearch-search-program-options '("-d")
  "*Options for searching the query for `wdsgrep.exe'."
  :type '(repeat :tag "Options" string)
  :group 'ndwinsearch)

(defcustom ndwinsearch-search-program-count-options '("-c")
  "*Options for counting the hit of query for  `wdsgrep.exe'."
  :type '(repeat :tag "Options" string)
  :group 'ndwinsearch)

(defcustom ndwinsearch-heading-regexp 
  '("^.+\\\\\\([^\\]+\\)\\(\\\\[^\\]+\\)$"  . "\\1\\2")
  "*Regular Expressions for Headings of file name."
  :type '(cons :tag "Regexp" string :tag "Replace" string)
  :group 'ndwinsearch)

(defcustom ndwinsearch-coding-system 'cp932-dos
  "*Coding system to be used with  `wdsgrep.exe'."
  :type 'symbol
  :group 'ndwinsearch)

(defcustom ndwinsearch-view-program "cmd.exe"
  "*Command name of `cmd.exe'."
  :type 'string
  :group 'ndwinsearch)

(defcustom ndwinsearch-view-program-options '("/c" "start" "")
  "*Options for `cmd.exe'."
  :type '(repeat :tag "Options" string)
  :group 'ndwinsearch)

(defcustom ndwinsearch-title "Windows Search"
  "*Title of ndwinsearch dictionary."
  :type 'string
  :group 'ndwinsearch)

;;(defcustom ndwinsearch-max-chars (* 6 lookup-max-text)
;;  "*Maximum bytes of display.  Unicode escape chars takes up to 6 bytes."
;;  :type 'integer
;;  :group 'ndwinsearch)


;;;
;;; Internal Variables
;;;

(defvar ndwinsearch-link-map nil)
(unless ndwinsearch-link-map
  (setq ndwinsearch-link-map (copy-keymap lookup-content-mode-map))
  (define-key ndwinsearch-link-map "\C-m" 'ndwinsearch-follow-link)
  (define-key ndwinsearch-link-map [mouse-2] 'ndwinsearch-mouse-follow))

;;;
;;; types
;;;

(put 'ndwinsearch :methods 
     '(exact prefix substring text))
(put 'ndwinsearch :arranges 
     '((structure ndwinsearch-arrange-structure)))
(put 'ndwinsearch ':default-method
     'text)


;;;
;;; Interface functions
;;;

(put 'ndwinsearch :list #'ndwinsearch-list)
(defun ndwinsearch-list (agent)
  (if (executable-find ndwinsearch-search-program)
      (list (lookup-new-dictionary agent ""))
    (message "ndwinsearch: error.  `wdsgrep.exe' not found.")
    nil))

(put 'ndwinsearch :title ndwinsearch-title)

(put 'ndwinsearch :search #'ndwinsearch-search)
(defun ndwinsearch-search (dictionary query)
  "Search winsearch DICTIONARY for QUERY."
  (let* ((location
          (lookup-agent-location
           (lookup-dictionary-agent dictionary)))
         (extension (lookup-dictionary-option dictionary ":ext"))
         (query-method  (lookup-query-method query))
         (query-pattern (lookup-query-pattern query))
         (arguments (list query-pattern))
         count entry entries)
    (if location
        (setq arguments (nconc (list "-p" location) arguments)))
    (if extension
        (setq arguments (nconc (list "-e" extension) arguments)))
    ;; count the number of hits.  not supported for now
    ;;(with-temp-buffer
    ;;  (lookup-with-coding-system 
    ;;    (apply 'call-process ndwinsearch-search-program
    ;;           nil t nil (append ndwinsearch-search-program-count-options
    ;;                             arguments)))
    ;;  (goto-char (point-min))
    ;;  (re-search-forward "[0-9]+" nil t)
    ;;  (setq count (string-to-number (match-string 0))))
    (setq count lookup-max-hits) ;; remove later
    (if (> count lookup-max-hits)
        (progn
          (message "Too many hits! %d" count)
          nil)
      (with-temp-buffer
        (lookup-with-coding-system ndwinsearch-coding-system
          (apply 'call-process ndwinsearch-search-program
                 nil t nil (append (list "-n" (number-to-string lookup-max-hits))
                                   ndwinsearch-search-program-options
                                   arguments)))
        (goto-char (point-min))
        (while (re-search-forward "^\\(.+?\\);.+$" nil t)
          (let ((code (match-string 0))
                (fname (match-string 1)))
            (setq entry
                  (lookup-new-entry 
                   'regular dictionary code 
                   (replace-regexp-in-string 
                    (car ndwinsearch-heading-regexp)
                    (cdr ndwinsearch-heading-regexp)
                    fname))))
          (setq entries (cons entry entries)))
        (nreverse entries)))))

(put 'ndwinsearch :content #'ndwinsearch-content)
(defun ndwinsearch-content (entry)
  (let* ((code    (lookup-entry-code entry))
         point)
    (string-match "^\\(.+?\\);\\(.+\\)$" code)
    (concat (match-string 1 code) "\n" (match-string 2 code))))

(defun ndwinsearch-arrange-structure (entry)
  (ndwinsearch-set-link (point) (line-end-position) 
                        (buffer-substring (point) (line-end-position)))
  )

(defun ndwinsearch-set-link (start end file)
  (add-text-properties start end
                       (list
                        'keymap ndwinsearch-link-map
                        'face 'lookup-reference-face
                        'mouse-face 'highlight
                        'lookup-tab-stop t
                        'ndwinsearch-link file)))

(defun ndwinsearch-get-link (&optional pos)
  (get-text-property (or pos (point)) 'ndwinsearch-link))

(defun ndwinsearch-follow-link ()
  (interactive)
  (let ((file (ndwinsearch-get-link (point))))
    (apply 'call-process ndwinsearch-view-program 
           nil 0 nil
           (append ndwinsearch-view-program-options
                   (list file)))))

(defun ndwinsearch-mouse-follow (event)
  "Play the binary you click on."
  (interactive "e")
  (mouse-set-point event)
  (ndwinsearch-follow-link))

(provide 'ndwinsearch)

;;; ndwinsearch.el ends here
