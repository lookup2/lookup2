;;; lookup-vars.el --- Lookup global variables -*- lexical-binding: t -*-
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

(declare-function lookup-dictionary-title "lookup-types" (dictionary))
(declare-function lookup-entry-heading "lookup-types" (entry))
(declare-function lookup-entry-dictionary "lookup-types")

;;;;;;;;;;;;;;;;;;;;
;; Custom Variables
;;;;;;;;;;;;;;;;;;;;

(defgroup lookup nil
  "Search interface to electronic dictionaries."
  :group 'applications)

;;;
;;; Setup variables
;;;

(defgroup lookup-setup-variables nil
  "Setup variables."
  :group 'lookup)

(defcustom lookup-enable-splash t
  "*Non-nil displays splash screen if available."
  :type 'boolean
  :group 'lookup-setup-variables)

(defcustom lookup-init-directory (concat user-emacs-directory "/lookup")
  "*Lookup initialization directory."
  :type 'file
  :group 'lookup-setup-variables)

(defcustom lookup-init-file (expand-file-name "init.el" lookup-init-directory)
  "*Lookup initialization file."
  :type 'file
  :group 'lookup-setup-variables)

(defcustom lookup-support-autoload-alist nil
  "*Alist of load definitions of support files.
Each element looks like (REGEXP . FILE), where REGEXP is a regexp
that matches a dictionary ID, and FILE is a support file name."
  :type '(repeat (cons (string :tag "regexp") (string :tag "file")))
  :group 'lookup-setup-variables)

(defcustom lookup-mode-module-alist nil
  "*Alist of major modes and module names."
  :type '(repeat (cons (symbol :tag "mode") (string :tag "module")))
  :group 'lookup-setup-variables)

;;;
;;; General options
;;;

(defgroup lookup-general-options nil
  "General customizable variables."
  :group 'lookup)

(defcustom lookup-default-method 'exact
  "*Default search method."
  :type 'symbol
  :group 'lookup-general-options)

(defcustom lookup-initial-memorandum
  (lambda (entry)
    (format "Title: %s\nEntry: %s\nDate: %s\n\n"
            (lookup-dictionary-title (lookup-entry-dictionary entry))
            (lookup-entry-heading entry)
            (format-time-string "%a, %e %b %Y %T %z")))
  "*Initial memorandum."
  :type 'function
  :group 'lookup-general-options)

(defcustom lookup-cite-header nil
  "*Header string on citing entry texts.
If this string contains \"%T\", it is replaced by the title of
the dictionary.  Dictionary option `cite-header' overrides this variable."
  :type 'string
  :group 'lookup-general-options)

(defcustom lookup-cite-prefix nil
  "*Prefix string on citing entry texts.
Dictionary option `cite-prefix' overrides this variable."
  :type 'string
  :group 'lookup-general-options)

(defcustom lookup-gaiji-alternative "_"
  "*Default gaiji alternative string."
  :type 'string
  :group 'lookup-general-options)

(defcustom lookup-process-coding-system
  (if (memq system-type '(ms-dos windows-nt OS/2 emx))
      'cp932
    'utf-8)
  "*Default coding system for external processes."
  :type 'symbol
  :group 'lookup-general-options)

(defcustom lookup-max-hits 120
  "*Maximum entries to display for each Agent.
0 means unlimited."
  :type 'integer
  :group 'lookup-general-options)

(defcustom lookup-max-text 10000
  "*Maximum length of entry texts.
0 means unlimited."
  :type 'integer
  :group 'lookup-general-options)

(defcustom lookup-max-history 80
  "*Maximum number of sessions in search history.
0 means unlimited."
  :type 'integer
  :group 'lookup-general-options)

(defcustom lookup-edit-input t
  "If t, word or region to be searched will be filtered and edited."
  :type 'boolean
  :group 'lookup-general-options)

;; from 1.4.1
(defcustom lookup-inline-image t
  "t ならば (可能な場合に) 画像を表示する。"
  :type 'boolean
  :group 'lookup-general-options)

(defcustom lookup-max-image-size 67108864
  "検索時に表示する画像の最大サイズ (バイト)。
デフォルトは 64MB。nil を指定すると、無制限になる。"
  :type 'integer
  :group 'lookup-general-options)

(defcustom lookup-reference-default-caption "参照"
  "A string to be used when the caption of reference is empty."
  :type 'string
  :group 'lookup-general-options)

;;;
;;; View options
;;;

(defgroup lookup-view-options nil
  "Look and feel."
  :group 'lookup)

(defcustom lookup-fill-column .9
  "*Width (in integer) or ratio (in decimal) when filling lines."
  :type 'number
  :group 'lookup-view-options)

(defcustom lookup-window-height 4
  "*Height of the Lookup main window."
  :type 'number
  :group 'lookup-view-options)

(defcustom lookup-title-width 26
  "*Dictionary title width."
  :type 'integer
  :group 'lookup-view-options)

(defcustom lookup-frame-alist
  '((title . "Lookup") (menu-bar-lines . 0) (width . 48) (height . 32)
    (lookup-fill-column . 45))
  "*frame-alist for Lookup frames."
  :type '(repeat (cons :tag "Parameter"
                       (symbol :tag "tag")
                       (sexp :tag "value")))
  :group 'lookup-view-options)

;;;
;;; Additional features
;;;

(defgroup lookup-additional-features nil
  "Additional features."
  :group 'lookup)

(defcustom lookup-mecab-program "mecab"
  "*Program name of MECAB."
  :type 'string
  :group 'lookup-additional-features)

(defcustom lookup-mecab-coding-system lookup-process-coding-system
  "*Coding system for MECAB."
  :type 'symbol
  :group 'lookup-additional-features)

(defcustom lookup-text-segmentize-japanese
  (if (executable-find lookup-mecab-program) 'mecab)
  "*Symbol `mecab' enables Japanese Segmentation by MECAB."
  :type 'symbol
  :group 'lookup-additional-features)

(defcustom lookup-enable-format t
  "Non-nil enables formatting text."
  :type 'boolean
  :group 'lookup-additional-features)

(defcustom lookup-enable-gaiji t
  "*Non-nil enables displaying gaijis."
  :type 'boolean
  :group 'lookup-additional-features)

(defcustom lookup-enable-example t
  "*Non-nil enables displaying examples."
  :type 'boolean
  :group 'lookup-additional-features)

(defcustom lookup-enable-debug nil
  "*Non-nil enables debug features."
  :type 'boolean
  :group 'lookup-additional-features)

;;;
;;; Search agents
;;;

(defcustom lookup-search-agents nil
  "*List of Search Agents.

Each List has the following format::

  (CLASS LOCATION [KEY1 VALUE1 [KEY2 VALUE2 [...]]])

Where 
- CLASS specifies agent class by a symbol.
- LOCATION specifies a location of agent by a string.
- KEY and VALUE are options for agent.

Example: (setq lookup-search-agents
              '((ndwnj \"/path/to/wnjpn.db\")
                (ndeb \"/cdrom\" :enable (\"EIWA\")))))"
  :type '(repeat (sexp :tag "agent"))	; type is a bit complex..
  :group 'lookup)

;;;
;;; Caches
;;;

(defgroup lookup-cache nil
  "Cache control."
  :group 'lookup)

(defcustom lookup-cache-file
  (expand-file-name 
   (concat "cache-" 
           (replace-regexp-in-string "\\..+" "" system-name)
           ".el") lookup-init-directory)
  "*Lookup disk cache file."
  :type 'file
  :group 'lookup-cache)

;;;
;;; Faces
;;;

(defgroup lookup-faces nil
  "Faces."
  :group 'lookup)

(defface lookup-heading-1-face
    '((((class color) (background light)) (:foreground "SlateBlue" :bold t))
      (((class color) (background dark)) (:foreground "LightBlue" :bold t)))
  "Level 1 heading face."
  :group 'lookup-faces)

(defface lookup-heading-2-face
  '((((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t)))
  "Level 2 heading face."
  :group 'lookup-faces)

(defface lookup-heading-3-face
  '((((class color) (background light)) (:foreground "Orange" :bold t))
    (((class color) (background dark)) (:foreground "LightSalmon" :bold t)))
  "Level 3 heading face."
  :group 'lookup-faces)

(defface lookup-heading-4-face
  '((t (:bold t)))
  "Level 4 heading face."
  :group 'lookup-faces)

(defface lookup-heading-5-face
  '((t nil))
  "Level 5 heading face."
  :group 'lookup-faces)

(defface lookup-comment-face
  '((((class color) (background light)) (:foreground "Grey"))
    (((class color) (background dark)) (:foreground "LightGrey")))
  "Comment face."
  :group 'lookup-faces)

(defface lookup-bold-face
  '((t (:weight bold)))
  "Face used to bold text."
  :group 'lookup-faces)

(defface lookup-italic-face
  '((t (:slant italic)))
  "Italic face."
  :group 'lookup-faces)

(defface lookup-emphasis-face
  '((t (:slant italic :weight bold)))
  "Face used to emphasized text."
  :group 'lookup-faces)

(defface lookup-reference-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "Cyan")))
  "Face used to highlight reference."
  :group 'lookup-faces)

(defface lookup-referred-face
  '((((class color) (background light)) (:foreground "DarkViolet"))
    (((class color) (background dark)) (:foreground "Plum")))
  "Face used to highlight refered reference."
  :group 'lookup-faces)


;;;;;;;;;;;;;;;;;;;;
;; Advanced Variables
;;;;;;;;;;;;;;;;;;;;

(defvar lookup-search-modules nil)
(defvar lookup-agent-option-alist nil)
(defvar lookup-dictionary-option-alist nil)
(defvar lookup-support-alist nil)

(defvar lookup-support-agent nil
  "Symbol indicating the search agent that a support file applies to.
This variable is automatically set when loading a support file, and
should be only refered in support files.")

(defvar lookup-support-agent-options nil
  "Symbol indicating the options of search agent that a support file applies to.
This variable is automatically set when loading a support file,
and should be only refered in support files.")

(defvar lookup-support-dictionary-id ""
  "Symbol indicating the dictionary that a support file applies to.
This variable is automatically set when loading a support file,
and should be only refered in support files.")

(defvar lookup-support-options nil
  "Dictionary options defined in a support file.
This variable should be only set in support files.")

(defvar lookup-arrange-table
  '((replace   lookup-arrange-replaces)
    (gaiji     lookup-arrange-gaijis)
    (reference lookup-arrange-references)
    (structure lookup-arrange-structure)
    (fill      lookup-arrange-fill-lines)))

;;;
;:: Hooks
;;;

(defvar lookup-load-hook nil
  "*List of functions called after loading Lookup.
This hook will run just after loading `lookup-init-file' and
`lookup-cache-file'.")

(defvar lookup-after-dictionary-search-hook nil)

;;;
;;; Command control
;;;

(defvar lookup-force-update nil)
(defvar lookup-open-function 'lookup-other-window)


;;;;;;;;;;;;;;;;;;;;
;; Internal Variables
;;;;;;;;;;;;;;;;;;;;

(defvar lookup-agent-list nil)
(defvar lookup-module-list nil)
(defvar lookup-dictionary-list nil)
(defvar lookup-entry-table nil)
(defvar lookup-buffer-list nil)
(defvar lookup-current-session nil)
(defvar lookup-last-session nil)
(defvar lookup-record-table nil)
(defvar lookup-search-history nil)

(defvar lookup-help-message nil)
(make-variable-buffer-local 'lookup-help-message)

(defvar lookup-byte-compiling nil)
(defvar lookup-dynamic-display nil)
(defvar lookup-search-dictionaries nil)
(defvar lookup-window-configuration nil)

(defvar lookup-support-autoload-default-alist nil)

(defvar lookup-query-filters nil
  "Functions to be applied before a QUERY is queried.")

(defvar lookup-url-regexp
  "\\(https?://\\|file://\\|javascript:\\)[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+"
  "Regular expression to search within contents.")

;; lookup-select module
(defvar lookup-select-module nil)

;;;
;;; Global commands
;;;

(defvar lookup-global-map nil)

(unless lookup-global-map
  (setq lookup-global-map (make-sparse-keymap))
  (define-key lookup-global-map "\C-\M-n" 'lookup-next-history)
  (define-key lookup-global-map "\C-\M-p" 'lookup-previous-history)
  (define-key lookup-global-map "\C-\M-f" 'lookup-forward-module)
  (define-key lookup-global-map "\C-\M-b" 'lookup-backward-module)
  (define-key lookup-global-map "B" 'lookup-list-bookmarks)
  (define-key lookup-global-map "H" 'lookup-list-history)
  (define-key lookup-global-map "f" 'lookup-find-pattern)
  (define-key lookup-global-map "o" 'lookup-open-window)
  (define-key lookup-global-map "r" 'lookup-return)
  (define-key lookup-global-map "q" 'lookup-suspend)
  (define-key lookup-global-map "Q" 'lookup-exit)
  (define-key lookup-global-map "R" 'lookup-restart)
  (define-key lookup-global-map "?" 'lookup-help))

(provide 'lookup-vars)

;;; lookup-vars.el ends here

;;; Local variables:
;;; mode:emacs-lisp
;;; End:
