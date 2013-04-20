;;; ndwdsgrep.el --- search agent for `Windows Search'
;; Copyright (C) 2009,2013 Taichi KAWABATA <kawabata.taichi@gmail.com>

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
;; http://ext.noue.org/ext/wdsgrep/
;; https://github.com/inouetmhr/wdsgrep
;;
;;
;; Example:
;; 
;; (setq lookup-search-agents
;;       '(....
;;         (ndwdsgrep "C:/Documents and Settings/") ;; Search for specified directory
;;         (ndwdsgrep "D:/" :ext ".log")             ;; Search for specified directory with extension ".log".
;;        ))

;;; Code:

(require 'lookup)

;;;
;;; Customizable variables
;;;

(defgroup ndwdsgrep nil
  "Lookup Windows Search Interface."
  :group 'lookup-search-agents)

(defcustom ndwdsgrep-search-program "D:\\wdsgrep-4.1test\\wdsgrep.exe"
  "*Command name and location of `wdsgrep.exe'."
  :type 'string
  :group 'ndwdsgrep)

(defcustom ndwdsgrep-search-program-options '("-d")
  "*Options for searching the query for `wdsgrep.exe'."
  :type '(repeat :tag "Options" string)
  :group 'ndwdsgrep)

(defcustom ndwdsgrep-search-program-count-options '("-c")
  "*Options for counting the hit of query for  `wdsgrep.exe'."
  :type '(repeat :tag "Options" string)
  :group 'ndwdsgrep)

(defcustom ndwdsgrep-heading-regexp 
  '("^.+\\\\\\([^\\]+\\)\\(\\\\[^\\]+\\)$"  . "\\1\\2")
  "*Regular Expressions for Headings of file name."
  :type '(cons :tag "Regexp" string :tag "Replace" string)
  :group 'ndwdsgrep)

(defcustom ndwdsgrep-coding-system 'cp932-dos
  "*Coding system to be used with  `wdsgrep.exe'."
  :type 'symbol
  :group 'ndwdsgrep)

(defcustom ndwdsgrep-view-program "cmd.exe"
  "*Command name of `cmd.exe'."
  :type 'string
  :group 'ndwdsgrep)

(defcustom ndwdsgrep-view-program-options '("/c" "start" "")
  "*Options for `cmd.exe'."
  :type '(repeat :tag "Options" string)
  :group 'ndwdsgrep)

(defcustom ndwdsgrep-title "Windows Search"
  "*Title of ndwdsgrep dictionary."
  :type 'string
  :group 'ndwdsgrep)

;;(defcustom ndwdsgrep-max-chars (* 6 lookup-max-text)
;;  "*Maximum bytes of display.  Unicode escape chars takes up to 6 bytes."
;;  :type 'integer
;;  :group 'ndwdsgrep)


;;;
;;; Internal Variables
;;;

(defvar ndwdsgrep-link-map nil)
(unless ndwdsgrep-link-map
  (setq ndwdsgrep-link-map (copy-keymap lookup-content-mode-map))
  (define-key ndwdsgrep-link-map "\C-m" 'ndwdsgrep-follow-link)
  (define-key ndwdsgrep-link-map [mouse-2] 'ndwdsgrep-mouse-follow))

;;;
;;; types
;;;

(put 'ndwdsgrep :methods 
     '(exact prefix substring text))
(put 'ndwdsgrep :arranges 
     '((structure ndwdsgrep-arrange-structure)))
(put 'ndwdsgrep ':default-method
     'text)


;;;
;;; Interface functions
;;;

(put 'ndwdsgrep :list 'ndwdsgrep-list)
(defun ndwdsgrep-list (agent)
  (if (executable-find ndwdsgrep-search-program)
      (list (lookup-new-dictionary agent ""))
    (message "ndwdsgrep: error.  `wdsgrep.exe' not found.")
    nil))

(put 'ndwdsgrep :title ndwdsgrep-title)

(put 'ndwdsgrep :search 'ndwdsgrep-search)
(defun ndwdsgrep-search (dictionary query)
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
    ;;    (apply 'call-process ndwdsgrep-search-program
    ;;           nil t nil (append ndwdsgrep-search-program-count-options
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
        (lookup-with-coding-system ndwdsgrep-coding-system
          (apply 'call-process ndwdsgrep-search-program
                 nil t nil (append (list "-n" (number-to-string lookup-max-hits))
                                   ndwdsgrep-search-program-options
                                   arguments)))
        (goto-char (point-min))
        (while (re-search-forward "^\\(.+?\\);.+$" nil t)
          (let ((code (match-string 0))
                (fname (match-string 1)))
            (setq entry
                  (lookup-new-entry 
                   'regular dictionary code 
                   (replace-regexp-in-string 
                    (car ndwdsgrep-heading-regexp)
                    (cdr ndwdsgrep-heading-regexp)
                    fname))))
          (setq entries (cons entry entries)))
        (nreverse entries)))))

(put 'ndwdsgrep :content 'ndwdsgrep-content)
(defun ndwdsgrep-content (entry)
  (let* ((code    (lookup-entry-code entry))
         point)
    (string-match "^\\(.+?\\);\\(.+\\)$" code)
    (concat (match-string 1 code) "\n" (match-string 2 code))))

(defun ndwdsgrep-arrange-structure (entry)
  (ndwdsgrep-set-link (point) (line-end-position)
                        (buffer-substring (point) (line-end-position)))
  )

(defun ndwdsgrep-set-link (start end file)
  (add-text-properties start end
                       (list
                        'keymap ndwdsgrep-link-map
                        'face 'lookup-reference-face
                        'mouse-face 'highlight
                        'lookup-tab-stop t
                        'ndwdsgrep-link file)))

(defun ndwdsgrep-get-link (&optional pos)
  (get-text-property (or pos (point)) 'ndwdsgrep-link))

(defun ndwdsgrep-follow-link ()
  (interactive)
  (let ((file (ndwdsgrep-get-link (point))))
    (apply 'call-process ndwdsgrep-view-program 
           nil 0 nil
           (append ndwdsgrep-view-program-options
                   (list file)))))

(defun ndwdsgrep-mouse-follow (event)
  "Play the binary you click on."
  (interactive "e")
  (mouse-set-point event)
  (ndwdsgrep-follow-link))

(provide 'ndwdsgrep)

;;; ndwdsgrep.el ends here
