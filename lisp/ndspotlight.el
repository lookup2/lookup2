;;; ndspotlight.el --- search agent for MacOS SpotLight
;; Copyright (C) 2009 Taichi KAWABATA <kawabata.taichi@gmail.com>

;; Author: Taichi KAWABATA <kawabata.taichi@gmail.com>
;; Keywords: MacOS, spotlight, search, dictionary

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

;; This agent will search for the file with specific directory and 
;; displays text for target files.
;;
;; It should be specified with directory and target metadata name,
;; separated by `&'.  Metadata name is not mandatory.
;; (If you specify `:restrict' option, then such restriction may be
;; applied.)
;; 
;; Example:
;; 
;; (setq lookup-search-agents
;;       '(....
;;         (ndspotlight "/usr/doc")
;;        ))
;; /usr/bin/mdimport -n -d2 /path/to/file.{pdf,xls,doc,ppt} >& file.txt

;;; Code:

(require 'lookup)
;;(require 'normalize)

;;;
;;; Customizable variables
;;;

(defgroup ndspotlight nil
  "Lookup csrd interface."
  :group 'lookup-search-agents)

(defcustom ndspotlight-search-program "mdfind"
  "*Command name of `mdfind'."
  :type 'string
  :group 'ndspotlight)

(defcustom ndspotlight-search-program-options '()
  "*Command name of `mdfind'."
  :type '(repeat :tag "Options" string)
  :group 'ndspotlight)

(defcustom ndspotlight-search-modifiers "d"
  "*Search modifiers.  
`d' means insensitive to diacritical marks."
  :type 'string
  :group 'ndspotlight)

(defcustom ndspotlight-content-program "mdimport"
  "*Command name of `mdimport'."
  :type 'string
  :group 'ndspotlight)

(defcustom ndspotlight-content-program-options '("-d2")
  "*Options for content-program."
  :type '(repeat :tag "Options" string)
  :group 'ndspotlight)

(defcustom ndspotlight-view-program "qlmanage"
  "*Command name of `qlmanage'."
  :type 'string
  :group 'ndspotlight)

(defcustom ndspotlight-view-program-options '("-p")
  "*Options for view program."
  :type '(repeat :tag "Options" string)
  :group 'ndspotlight)

(defcustom ndspotlight-title "Spotlight"
  "*Title of ndspotlight dictionary."
  :type 'string
  :group 'ndspotlight)

(defcustom ndspotlight-max-chars 5000
  "*Maximum bytes of display.  Unicode escape chars takes up to 5 bytes."
  :type 'integer
  :group 'ndspotlight)


;;;
;;; Internal Variables
;;;

(defvar ndspotlight-link-map nil)
(unless ndspotlight-link-map
  (setq ndspotlight-link-map (copy-keymap lookup-content-mode-map))
  (define-key ndspotlight-link-map "\C-m" 'ndspotlight-follow-link)
  (define-key ndspotlight-link-map [mouse-2] 'ndspotlight-mouse-follow))

;;;
;;; types
;;;

(put 'ndspotlight :methods 
     '(exact prefix suffix substring wildcard keyword))
(put 'ndspotlight :arranges '((structure ndspotlight-arrange-structure)))


;;;
;;; Interface functions
;;;

(put 'ndspotlight :list #'ndspotlight-list)
(defun ndspotlight-list (agent)
  (list (lookup-new-dictionary agent "")))

(put 'ndspotlight :title ndspotlight-title)

(put 'ndspotlight :search #'ndspotlight-search)
(defun ndspotlight-search (dictionary query)
  (let ((options (or (lookup-dictionary-option dictionary :restrict t)))
        (string (lookup-query-string query))
        count entry entries)
    (with-temp-buffer
      (lookup-with-coding-system 'utf-8-mac
        (apply 'call-process ndspotlight-search-program 
               nil t nil "-count" (list string)))
      (goto-char (point-min))
      (re-search-forward "[0-9]+" nil t)
      (setq count (string-to-number (match-string 0))))
    (if (> count lookup-max-hits)
        (progn
          (message "Too many hits! %d" count)
          nil)
      (with-temp-buffer 
        (lookup-with-coding-system 'utf-8-mac
          (apply 'call-process ndspotlight-search-program 
                 nil t nil (list string)))
        (goto-char (point-min))
        (while (re-search-forward "^/.+/\\([^/\n]+\\)$" nil t)
          (setq entry 
                (lookup-new-entry 'regular 
                                  dictionary (match-string 0) (match-string 1)))
          (setq entries (cons entry entries)))
        (nreverse entries)))))

(put 'ndspotlight :content #'ndspotlight-content)
(defun ndspotlight-content (entry)
  (let* ((code (lookup-entry-code entry))
         (options (list "-n" "-d2" code))
         point)
    (with-temp-buffer
      (lookup-with-coding-system 'utf-8-mac
        (apply 'call-process ndspotlight-content-program 
               nil t nil options))
      (goto-char (point-min))
      (re-search-forward "kMDItemDisplayName\\|kMDItemTitle")
      (forward-line) (zap-to-char 3 ?\") (delete-region (point-min) (point))
      (forward-line) (setq point (point))
      (if (search-forward "kMDItemTextContent = \"")
          (delete-region point (point)))
      (while (re-search-forward "+" nil t) 
        (replace-match (if (= 1 (length (match-string 0))) "\n" "\n\n")))
      (goto-char ndspotlight-max-chars)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "\\\\U\\([0-9a-f]\\{4\\}\\)" nil t)
        (replace-match (char-to-string (string-to-int (match-string 1) 16))))
      (buffer-string))))

(defun ndspotlight-arrange-structure (entry)
  ;; nothing to do for now
  )

(provide 'ndspotlight)

;;; ndspotlight.el ends here
