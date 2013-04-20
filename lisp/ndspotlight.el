;;; ndspotlight.el --- search agent for MacOS SpotLight -*- lexical-binding: t -*-
;; Copyright (C) 2009 Taichi KAWABATA <kawabata.taichi@gmail.com>

;; Author: Taichi KAWABATA <kawabata.taichi@gmail.com>
;; Keywords: MacOS, spotlight, search

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

;; This agent will search for the file with specified directory and 
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
;;         (ndspotlight)
;;         (ndspotlight "/usr/doc")
;;         (ndspotlight "/usr/doc&kMDItemAuthors")
;;         (ndspotlight "/usr/doc"
;;                      :restrict "kMDItemContentType =='audio'wc")
;;        ))

;;; Code:

(require 'lookup)
(require 'lookup-content)
(require 'ucs-normalize)

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
  "*Command options of `mdfind'."
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

(defcustom ndspotlight-content-program-options '("-n -d2")
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

(defcustom ndspotlight-max-chars (* 6 lookup-max-text)
  "*Maximum bytes of display.  Unicode escape chars takes up to 6 bytes."
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
     '(exact prefix suffix substring text))
(put 'ndspotlight :arranges 
     '((structure ndspotlight-arrange-structure)))
(put 'ndspotlight ':default-method
     'text)


;;;
;;; Interface functions
;;;

(put 'ndspotlight :list 'ndspotlight-list)
(defun ndspotlight-list (agent)
  (if (executable-find ndspotlight-search-program)
      (list (lookup-new-dictionary agent ""))
    (message "ndspotlight: error.  executable program not found.")
    nil))

(put 'ndspotlight :title ndspotlight-title)

(put 'ndspotlight :search 'ndspotlight-search)
(defun ndspotlight-search (dictionary query)
  "Search spotlight DICTIONARY for QUERY."
  (let* ((location
          (lookup-agent-location
           (lookup-dictionary-agent dictionary)))
         (metadata (if (and location
                            (string-match "\\(.*\\)&\\(.*\\)" location))
                       (match-string 2 location)))
         (location (if (and location
                            (string-match "\\(.*\\)&\\(.*\\)" location))
                       (match-string 1 location) location))
         (location (if (equal location "") nil location))
         (restrict (lookup-dictionary-option dictionary :restrict t))
         (pattern  (concat (if restrict
                               (concat restrict " && "))
                           (if metadata
                               (concat metadata " == "))
                           "\""
                           (lookup-query-pattern query)
                           "\""
                           ndspotlight-search-modifiers
                           ))
         (arguments (if location
                        (list "-onlyin" location pattern)
                      (list pattern)))
         count entry entries)
    (with-temp-buffer
      (lookup-with-coding-system 'utf-8-mac
        (apply 'call-process ndspotlight-search-program
               nil t nil "-count" arguments))
      (goto-char (point-min))
      (re-search-forward "[0-9]+" nil t)
      (setq count (string-to-number (match-string 0))))
    (if (or (<= count lookup-max-hits)
            (y-or-n-p (format "SpotLight: %s are hit.  Do you want to display them all?" count)))
      (with-temp-buffer
        (lookup-with-coding-system 'utf-8-mac
          (apply 'call-process ndspotlight-search-program
                 nil t nil (append ndspotlight-search-program-options
                                   arguments)))
        (goto-char (point-min))
        (while (re-search-forward "^/.+/\\([^/\n]+\\)$" nil t)
          (setq entry
                (lookup-new-entry 
                 'regular dictionary (match-string 0)
                 (ucs-normalize-HFS-NFC-string (match-string 1))))
          (setq entries (cons entry entries)))
        (nreverse entries)))))

(put 'ndspotlight :content 'ndspotlight-content)
(defun ndspotlight-content (entry)
  (let* ((code (lookup-entry-code entry))
         (options (append ndspotlight-content-program-options
                          (list code))))
    (with-temp-buffer
      (lookup-with-coding-system 'utf-8
        (apply 'call-process ndspotlight-content-program 
               nil t nil options))
      (goto-char ndspotlight-max-chars)
      (delete-region (point) (point-max))
      (buffer-string))))

(defun ndspotlight-arrange-structure (entry)
  (let (title point)
    (setq tab-width 4)
    (when (re-search-forward "\\(kMDItemDisplayName\\|kMDItemTitle\\)[^{]+{[^=]+= \"\\(.+\\)\";" nil t)
      (setq title (match-string 2))
      (delete-region (point-min) (point))
      (insert title "\n"))
    (insert (lookup-entry-code entry) "\n")
    (setq point (point))
    (insert "《→ View》") (ndspotlight-set-link point (point) (lookup-entry-code entry))
    (forward-line) (setq point (point))
    (if (search-forward "kMDItemTextContent = \"" nil t)
        (delete-region point (point)))
    (while (re-search-forward "\\(\\|\\\\[nv]\\)+" nil t)
      (replace-match (if (= 1 (length (match-string 0))) "\n" "\n\n")))
    (goto-char (point-min))
    (while (re-search-forward "\\\\t" nil t)
      (replace-match "\t"))
    (goto-char (point-min))
    (while (re-search-forward "\\\\U\\([0-9a-f]\\{4\\}\\)" nil t)
      (replace-match (char-to-string (string-to-number (match-string 1) 16))))
    (ucs-normalize-HFS-NFC-region (point-min) (point-max))
    ))

(defun ndspotlight-set-link (start end file)
  (add-text-properties start end
                       (list
                        'keymap ndspotlight-link-map
                        'face 'lookup-reference-face
                        'mouse-face 'highlight
                        'lookup-tab-stop t
                        'ndspotlight-link file)))

(defun ndspotlight-get-link (&optional pos)
  (get-text-property (or pos (point)) 'ndspotlight-link))

(defun ndspotlight-follow-link ()
  (interactive)
  (let ((file (ndspotlight-get-link (point))))
    (apply 'call-process ndspotlight-view-program 
           nil 0 nil
           (append ndspotlight-view-program-options
                   (list file)))))

(defun ndspotlight-mouse-follow (event)
  "Play the binary you click on."
  (interactive "e")
  (mouse-set-point event)
  (ndspotlight-follow-link))

(provide 'ndspotlight)

;;; ndspotlight.el ends here
