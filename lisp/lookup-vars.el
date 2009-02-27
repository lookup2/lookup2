;;; lookup-vars.el --- Lookup global variables
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

(defcustom lookup-init-directory (expand-file-name "~/.lookup")
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
      'sjis-dos
    'euc-jp)
  "*Default coding system for external processes."
  :type 'symbol
  :group 'lookup-general-options)

(defcustom lookup-max-hits 50
  "*Maximum entries to display.
0 means unlimited."
  :type 'integer
  :group 'lookup-general-options)

(defcustom lookup-max-text 100000
  "*Maximum length of entry texts.
0 means unlimited."
  :type 'integer
  :group 'lookup-general-options)

(defcustom lookup-max-history 80
  "*Maximum number of sessions in search history.
0 means unlimited."
  :type 'integer
  :group 'lookup-general-options)

;; from 1.4.1
(defcustom lookup-inline-image t
  "t ならば (可能な場合に) 画像を表示する。"
  :type 'boolean
  :group 'lookup-general-options)

(defcustom lookup-max-image-size 1048576
  "検索時に表示する画像の最大サイズ (バイト)。
デフォルトは 1MB。nil を指定すると、無制限になる。"
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

(defcustom lookup-title-width 24
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

(defcustom lookup-kakasi-program "kakasi"
  "*Program name of KAKASI."
  :type 'string
  :group 'lookup-additional-features)

(defcustom lookup-kakasi-coding-system lookup-process-coding-system
  "*Coding system for KAKASI."
  :type 'symbol
  :group 'lookup-additional-features)

(defcustom lookup-use-kakasi
  (if (let ((load-path exec-path))
	(or (locate-library lookup-kakasi-program t)
	    (locate-library (concat lookup-kakasi-program ".exe") t))) t)
  "*Non-nil enables Kanji extraction by using KAKASI."
  :type 'boolean
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
  "*検索エージェントの設定のリスト。
リストの各要素は次の形式を取る:

  (CLASS LOCATION [KEY1 VALUE1 [KEY2 VALUE2 [...]]])

CLASS には、エージェントの種類をシンボルで指定する。
LOCATION には、エージェントの所在を文字列で指定する。
KEY 及び VALUE は省略可能で、エージェントに対するオプションを指定する。

例: (setq lookup-search-agents
          '((ndtp \"dserver\" :port 2010)
            (ndeb \"/cdrom\" :enable (\"EIWA\")))))"
  :type '(repeat (sexp :tag "agent"))	; type はちょっとややこしすぎ・・
  :group 'lookup)

;;;
;;; Caches
;;;

(defgroup lookup-cache nil
  "Cache control."
  :group 'lookup)

(defcustom lookup-cache-file
  (expand-file-name "cache.el" lookup-init-directory)
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

(defface lookup-reference-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "Cyan")))
  "Face used to highlight reference."
  :group 'lookup-faces)

(defface lookup-refered-face
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

(defvar lookup-support-options nil
  "Dictionary options defined in a support file.
This variable should be only set in support files.")

(defvar lookup-arrange-table
  '((replace          lookup-arrange-replaces)
    (gaiji            lookup-arrange-gaijis)
    ;(reference        lookup-arrange-media)
    (reference        lookup-arrange-references)
    (structure        lookup-arrange-structure)
    (fill             lookup-arrange-fill-lines)))

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

(defvar lookup-search-method nil)
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

(provide 'lookup-vars)

;;; lookup-vars.el ends here

;;; Local variables:
;;; mode:emacs-lisp
;;; End:
