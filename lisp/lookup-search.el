;;; lookup-search.el --- Lookup Search Session
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

(require 'lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lookup Search Session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'lookup-search-session 'display 'lookup-search-session-display)
(put 'lookup-search-session 'excursion 'lookup-search-session-excursion)

;;;###autoload
(defun lookup-search-session (module query)
  (let* ((lookup-proceeding-message
	  (format "Looking up `%s'" (lookup-query-pattern query)))
	 (lookup-dynamic-display t)
	 (method (lookup-query-method query))
	 (original-module module)
	 search-found valid-dictionary priority entries)
    (lookup-proceeding-message nil)
    (lookup-start-session 'lookup-search-session module
      (while module
	(lookup-foreach
	 (lambda (dict)
	   (setq priority (lookup-module-dictionary-priority module dict))
	   (when (and (or lookup-valid-dictionaries
			  (cond ((eq priority t) t)
				((eq priority 'secondary) (not search-found))
				((eq priority 'supplement) search-found)))
		      (or (eq method 'default)
			  (memq method (lookup-dictionary-methods dict))))
	     (setq valid-dictionary t)
	     (lookup-proceeding-message
	      (format "by %s..." (lookup-dictionary-title dict)))
	     (when (setq entries (lookup-dictionary-search dict query))
	       (if search-found
		   (lookup-search-session-append lookup-current-session
						 entries)
		 (setq search-found t)
		 (lookup-session-set-query lookup-current-session query)
		 (lookup-session-set-entries lookup-current-session entries)
		 (lookup-session-set-dictionaries lookup-current-session
						  lookup-valid-dictionaries)
		 (lookup-open-session lookup-current-session)))))
	 (or lookup-valid-dictionaries (lookup-module-dictionaries module)))
	(if (or search-found (eq (setq module (lookup-nth-module 1 module))
				 original-module))
	    (setq module nil)))
      (if (not valid-dictionary)
	  (signal 'lookup-no-valid-dictionary (list method)))
      (if (not search-found)
	  (signal 'lookup-no-entry-error (list (lookup-query-pattern query)))))
    (lookup-proceeding-message t)))

;;;###autoload
(defun lookup-display-entries (module query entries)
  (lookup-start-session 'lookup-search-session module
    (lookup-session-set-query lookup-current-session query)
    (lookup-session-set-entries lookup-current-session entries)))

;;;
;;; Construct buffer
;;;

(defun lookup-search-session-display (session)
  (with-current-buffer (lookup-open-buffer (lookup-summary-buffer))
    (let ((query (lookup-session-query session))
	  (entries (lookup-session-entries session))
	  (excursion (lookup-session-excursion session)))
      ;; insert entries
      (let ((inhibit-read-only t))
	(lookup-summary-mode)
	(erase-buffer)
	(lookup-foreach 'lookup-search-session-insert entries)
	(set-buffer-modified-p nil))
      ;; set mode line
      (setq lookup-summary-line-module
	    (lookup-module-name (lookup-session-module session)))
      (setq lookup-summary-line-pattern (lookup-query-pattern query))
      (setq lookup-summary-line-number (number-to-string (length entries)))
      ;; display buffer
      (if excursion
	  (lookup-search-session-set-excursion excursion)
	(lookup-pop-to-buffer (current-buffer))
	(goto-char (point-min))
	(lookup-summary-goto-link)
	(if lookup-dynamic-display (sit-for 0))
	(lookup-summary-display-content)
	(if lookup-dynamic-display (sit-for 0))))))

(defun lookup-search-session-append (session entries)
  (with-current-buffer (lookup-summary-buffer)
    (save-excursion
      (let ((inhibit-read-only t)
	    (modified (buffer-modified-p)))
	(goto-char (point-max))
	(lookup-foreach 'lookup-search-session-insert entries)
	(set-buffer-modified-p modified)))
    (let ((entries (append (lookup-session-entries session) entries)))
      (lookup-session-set-entries session entries))
    (setq lookup-summary-line-number
	  (number-to-string (+ (string-to-number lookup-summary-line-number)
			       (length entries))))
    (if lookup-dynamic-display (sit-for 0))))

(defun lookup-summary-expand-references (entry)
  (let ((entries (lookup-entry-references entry)))
    ;; rebuild buffer
    (let ((inhibit-read-only t)
	  (modified (buffer-modified-p))
	  (start (progn (beginning-of-line) (point))))
      (delete-region start (progn (forward-line) (point)))
      (lookup-foreach 'lookup-search-session-insert entries)
      (goto-char start)
      (lookup-summary-goto-link)
      (set-buffer-modified-p modified))
    ;; rebuild cache
    (let ((list (lookup-session-entries (lookup-current-session))))
      (if (eq entry (car list))
	  (lookup-session-set-entries (lookup-current-session)
				      (append entries (cdr list)))
	(while (not (eq entry (cadr list))) (setq list (cdr list)))
	(if list (setcdr list (append entries (cddr list))))))
    (setq lookup-summary-line-number
	  (number-to-string (+ (string-to-number lookup-summary-line-number)
			       (1- (length entries)))))))

(defun lookup-search-session-insert (entry)
  (lookup-search-session-insert-mark entry)
  (insert (lookup-dictionary-head (lookup-entry-dictionary entry)) " \n")
  (backward-char)
  (lookup-entry-heading-insert entry)
  (forward-char))

(defun lookup-search-session-insert-mark (entry)
  (let ((bookmark (lookup-entry-bookmark (or (lookup-entry-substance entry)
					     entry))))
    (insert (if (stringp bookmark) "#" (if bookmark "!" " ")))))

;; content

(defun lookup-content-display (entry)
  (with-current-buffer (lookup-open-buffer (lookup-content-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((content (lookup-entry-get-property entry 'content)))
	(if (and content lookup-enable-format (not lookup-force-update))
	    (insert content)
	  (let ((lookup-proceeding-message
		 (format "Inserting `%s'" (lookup-entry-heading entry)))
		(dictionary (lookup-entry-dictionary entry)))
	    (lookup-proceeding-message nil)
	    (insert (lookup-entry-content entry))
	    (when lookup-enable-format
	      (lookup-arrange-content entry)
	      (lookup-entry-put-property entry 'content (buffer-string)))
	    (lookup-proceeding-message t)))
	(if lookup-enable-format (lookup-adjust-content entry))
	(if lookup-enable-record (lookup-record-entry-displayed entry)))
      ;; arrange functions might change the buffer mode
      (lookup-content-mode)
      (set-buffer-modified-p nil))
    (setq lookup-content-entry entry)
    (setq lookup-content-line-heading (lookup-entry-heading entry))
    (lookup-display-buffer (current-buffer))))

;;;
;;; Excursion
;;;

(defun lookup-search-session-excursion ()
  (let ((entry (get-buffer (lookup-summary-buffer)))
	(content (get-buffer (lookup-content-buffer))))
    (when entry
      (cons (with-current-buffer entry
	      (cons (point) (let ((window (get-buffer-window entry)))
			      (if window (window-start window)))))
	    (when (and content (with-current-buffer entry
				 (lookup-summary-this-entry)))
	      (with-current-buffer content
		(cons (point) (let ((window (get-buffer-window content)))
				(if window (window-start window))))))))))

(defun lookup-search-session-set-excursion (excursion)
  (let ((entry-point (caar excursion)) (entry-start (cdar excursion))
	(content (cdr excursion)))
    (lookup-pop-to-buffer (lookup-summary-buffer))
    (goto-char entry-point)
    (if entry-start
	(set-window-start (selected-window) entry-start))
    (if (eobp)
	(lookup-summary-previous-entry)
      (lookup-summary-display-content))
    (when content
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (car content))
	(if (cdr content)
	    (set-window-start (selected-window) (cdr content)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lookup Summary Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lookup-summary-mode-help
  "Lookup Summary mode:

`n' - next entry        `SPC' - next page       `<' - first page
`p' - previous entry    `DEL' - previous page   `>' - last page

`m' - mark entry        `!' - put bookmark
`u' - unmark entry      `#' - write memorandum  

`b' - auto bookmark

`M-p' - previous session
`M-n' - next session
`M-f' - forward module
`M-b' - backward module

`v'(iew)   - toggle Overview  	`o'(pen)  - maximize window
`s'(earch) - isearch-forward  	`w'(rite) - kill-ring-save contents

`L'(inks)  - list references    `S'(elect)   - select dictionaries
`M'(enu)   - display the menu   `B'(ookmark) - display the bookmarks

`f'(ind)
`c'(ontinuous)

`t f' - toggle format
`t g' - toggle gaiji
`t b' - toggle autobook

`g g' - search again            `g =' - redo exactly
`g s' - use secondary           `g @' - redo keyword
`g f' - use forward module      `g <' - redo prefix
`g b' - use backward module     `g >' - redo suffix

`r'   - 検索開始バッファに移動  `h'   - Content バッファに移動
`q'   - バッファを抜ける        `g'   - 検索をやり直す
`Q'   - Lookup を終了する       `R'   - Lookup を再起動する")

(defvar lookup-summary-mode-map nil
  "*Keymap for Lookup Summary mode.")

(unless lookup-summary-mode-map
  (setq lookup-summary-mode-map (make-sparse-keymap))
  (set-keymap-parent lookup-summary-mode-map lookup-global-map)
  ;; basic commands
  (define-key lookup-summary-mode-map " " 'lookup-summary-next-page)
  (define-key lookup-summary-mode-map "\C-?" 'lookup-summary-previous-page)
  (define-key lookup-summary-mode-map [delete] 'lookup-summary-previous-page)
  (define-key lookup-summary-mode-map "\C-m" 'lookup-summary-scroll-up-content)
  (define-key lookup-summary-mode-map "<" 'lookup-summary-beginning-of-content)
  (define-key lookup-summary-mode-map ">" 'lookup-summary-end-of-content)
  (define-key lookup-summary-mode-map "n" 'lookup-summary-next-entry)
  (define-key lookup-summary-mode-map "p" 'lookup-summary-previous-entry)
  (define-key lookup-summary-mode-map
    (if (featurep 'xemacs) 'button2 [mouse-2]) 'lookup-summary-mouse-follow)
  ;; entry management
  (define-key lookup-summary-mode-map "i" 'lookup-summary-info)
  (define-key lookup-summary-mode-map "m" 'lookup-summary-mark)
  (define-key lookup-summary-mode-map "u" 'lookup-summary-unmark)
  (define-key lookup-summary-mode-map "U" 'lookup-summary-unmark-all)
  (define-key lookup-summary-mode-map "!" 'lookup-summary-bookmark)
  (define-key lookup-summary-mode-map "#" 'lookup-summary-memorandum)
;  (define-key lookup-summary-mode-map "A" 'lookup-summary-add-entry)
;  (define-key lookup-summary-mode-map "C" 'lookup-summary-copy-entry)
;  (define-key lookup-summary-mode-map "D" 'lookup-summary-delete-entry)
;  (define-key lookup-summary-mode-map "E" 'lookup-summary-edit-entry)
;  (define-key lookup-summary-mode-map "O" 'lookup-summary-open-entry)
  ;; content access
  (define-key lookup-summary-mode-map "h" 'lookup-summary-content-window)
  (define-key lookup-summary-mode-map "s" 'lookup-summary-isearch-content)
  (define-key lookup-summary-mode-map "w" 'lookup-summary-cite-content)
  ;; toggle minor
  (define-key lookup-summary-mode-map "t" (make-sparse-keymap))
  (define-key lookup-summary-mode-map "l" 'lookup-summary-toggle-example)
  (define-key lookup-summary-mode-map "v" 'lookup-summary-toggle-overview)
  (define-key lookup-summary-mode-map "tb" 'lookup-summary-toggle-autobook)
  (define-key lookup-summary-mode-map "te" 'lookup-summary-toggle-example)
  (define-key lookup-summary-mode-map "tf" 'lookup-summary-toggle-format)
  (define-key lookup-summary-mode-map "tg" 'lookup-summary-toggle-gaiji)
  (define-key lookup-summary-mode-map "tv" 'lookup-summary-toggle-overview)
  ;; new session
  (define-key lookup-summary-mode-map "^" 'lookup-select-dictionary)
;  (define-key lookup-summary-mode-map "I" 'lookup-summary-show-index)
  (define-key lookup-summary-mode-map "L" 'lookup-summary-list-references)
  (define-key lookup-summary-mode-map "M" 'lookup-summary-dictionary-menu)
  ;; redo
  (define-key lookup-summary-mode-map "g" (make-sparse-keymap))
  (define-key lookup-summary-mode-map "gg" 'lookup-summary-redo)
  (define-key lookup-summary-mode-map "g=" 'lookup-summary-redo-exactly)
  (define-key lookup-summary-mode-map "g@" 'lookup-summary-redo-keyword)
  (define-key lookup-summary-mode-map "g>" 'lookup-summary-redo-prefix)
  (define-key lookup-summary-mode-map "g<" 'lookup-summary-redo-suffix)
  (define-key lookup-summary-mode-map "g." 'lookup-summary-update-content)
  (define-key lookup-summary-mode-map "1" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "2" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "3" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "4" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "5" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "6" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "7" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "8" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "9" 'lookup-summary-redo-nth-dictionary)
  (define-key lookup-summary-mode-map "0" 'lookup-summary-redo-all-dictionary)
  )

(defvar lookup-summary-mode-hook nil)

(defvar lookup-summary-overview-mode nil)
(make-variable-buffer-local 'lookup-summary-overview-mode)
(or (assq 'lookup-summary-overview-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lookup-summary-overview-mode " Overview")
				 minor-mode-alist)))

(defvar lookup-summary-autobook-mode nil)
(or (assq 'lookup-summary-autobook-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lookup-summary-autobook-mode " Autobook")
				 minor-mode-alist)))

(defvar lookup-summary-line-module "")
(defvar lookup-summary-line-pattern "")
(defvar lookup-summary-line-number "0")

(make-variable-buffer-local 'lookup-summary-line-module)
(make-variable-buffer-local 'lookup-summary-line-pattern)
(make-variable-buffer-local 'lookup-summary-line-number)

(defun lookup-summary-mode ()
  "\\{lookup-summary-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-summary-mode)
  (setq mode-name "Summary")
  (setq mode-line-buffer-identification
	'("Lookup:%b <" lookup-summary-line-module "> {"
	  lookup-summary-line-pattern "} ["
	  lookup-summary-line-number "]"))
  (setq lookup-mode-help lookup-summary-mode-help)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map lookup-summary-mode-map)
  (run-hooks 'lookup-summary-mode-hook))

;;;;;;;;;;;;;;;;;;;;
;; Interactive Commands
;;;;;;;;;;;;;;;;;;;;

(defun lookup-summary-display-content ()
  "ポイント行のエントリの本文を表示する。
Overview モードになっている場合にはそれを解除し、Content バッファを
オープンする。エントリがリファレンスの場合には、それを参照する。"
  (interactive)
  (lookup-summary-goto-link)
  (let ((entry (lookup-summary-this-entry)))
    (when entry
      (setq lookup-summary-overview-mode nil)
      (lookup-content-display entry)
      (if lookup-summary-autobook-mode (lookup-summary-bookmark)))))

(defun lookup-summary-mouse-follow (event)
  "マウスでクリックしたエントリの本文を表示する。"
  (interactive "e")
  (mouse-set-point event)
  (lookup-summary-display-content))

(defun lookup-summary-next-page ()
  "エントリ本文の表示を一ページ進める。
バッファの終わりまで達したら、次のエントリに移動する。"
  (interactive)
  (cond
   ((not (lookup-summary-this-entry)) nil)
   ((not (lookup-summary-content-visible-p)) (lookup-summary-display-content))
   ((lookup-with-buffer-and-window (lookup-content-buffer)
      (not (pos-visible-in-window-p (point-max) (selected-window))))
    (lookup-summary-scroll-up-content))
   (t (lookup-summary-next-entry))))

(defun lookup-summary-previous-page ()
  "エントリ本文の表示を一ページ戻す。
バッファの始めまで達したら、前のエントリに移動する。"
  (interactive)
  (cond
   ((not (lookup-summary-this-entry)) (lookup-summary-previous-entry))
   ((not (lookup-summary-content-visible-p)) (lookup-summary-display-content))
   ((lookup-with-buffer-and-window (lookup-content-buffer)
      (not (pos-visible-in-window-p (point-min) (selected-window))))
    (lookup-summary-scroll-down-content))
   (t (lookup-summary-previous-entry))))

(defun lookup-summary-scroll-up-content (&optional arg)
  "エントリ本文をプレフィスクの行数だけスクロール・アップする。"
  (interactive "p")
  (if (lookup-summary-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(scroll-up arg))
    (lookup-summary-display-content)))

(defun lookup-summary-scroll-down-content (&optional arg)
  "エントリ本文をプレフィスクの行数だけスクロール・ダウンする。"
  (interactive "p")
  (if (lookup-summary-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(scroll-down arg))
    (lookup-summary-display-content)))

(defun lookup-summary-beginning-of-content ()
  "エントリ本文の表示を先頭まで戻す。"
  (interactive)
  (if (lookup-summary-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (point-min)))
    (lookup-summary-display-content)))

(defun lookup-summary-end-of-content ()
  "エントリ本文の表示を末尾まで進める。"
  (interactive)
  (if (lookup-summary-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (point-max))
	(recenter -2))
    (lookup-summary-display-content)))

(defun lookup-summary-next-entry (&optional arg)
  "次のエントリを表示する。プレフィクスの数だけ進む。"
  (interactive "p")
  (if (eobp)
      (progn (message "End of buffer") (ding))
    (forward-line arg)
    (lookup-summary-goto-link)
    (unless lookup-summary-overview-mode
      (or (pos-visible-in-window-p (save-excursion (forward-line) (point)))
	  (recenter -2))
      (lookup-summary-display-content))))

(defun lookup-summary-previous-entry (&optional arg)
  "前のエントリを表示する。プレフィクスの数だけ戻る。"
  (interactive "p")
  (beginning-of-line)
  (if (bobp)
      (progn (message "Beginning of buffer") (ding))
    (forward-line (- (or arg 1)))
    (lookup-summary-goto-link)
    (unless lookup-summary-overview-mode
      (or (pos-visible-in-window-p (save-excursion (forward-line -1) (point)))
	  (recenter 1))
      (lookup-summary-display-content))))

(defun lookup-summary-info ()
  "エントリの情報を出力する。"
  (interactive)
  (let ((entry (lookup-summary-this-entry)))
    (with-current-buffer (lookup-open-buffer "*Entry Information*")
      (help-mode)
      (let ((inhibit-read-only t)
	    (dict (lookup-entry-dictionary entry))
	    (heading (lookup-entry-heading entry))
	    (memo (lookup-entry-bookmark entry)))
	(erase-buffer)
	(insert (format "Entry information for `%s':\n\n" heading))
	(insert (format "Dictionary: %s\n" (lookup-dictionary-id dict)))
	(insert (format "Code:       %s\n" (lookup-entry-code entry)))
	(if (stringp memo) (insert "\n---- memorandum ----\n" memo))
	(goto-char (point-min)))
      (lookup-display-buffer (current-buffer)))))

(defun lookup-summary-mark ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (delete-char 1)
      (insert "*")))
  (if lookup-summary-overview-mode (next-line 1)))

(defun lookup-summary-unmark ()
  (interactive)
  (let ((entry (lookup-summary-this-entry)) memo)
    (when entry
      (when (or (not (stringp (lookup-entry-bookmark entry)))
		(progn (setq memo (lookup-summary-memorandum-display))
		       (y-or-n-p "Are you sure to remove this memorandum? ")))
	(lookup-entry-set-bookmark entry nil)
	(lookup-module-remove-bookmark (lookup-current-module) entry)
	(lookup-summary-update-mark))
      (when memo
	(lookup-hide-buffer memo)
	(if (not lookup-summary-overview-mode)
	    (lookup-summary-display-content)))
      (if lookup-summary-overview-mode (next-line 1)))))

(defun lookup-summary-bookmark ()
  (interactive)
  (let ((entry (lookup-summary-this-entry)) memo)
    (when entry
      (when (or (not (stringp (lookup-entry-bookmark entry)))
		(progn (setq memo (lookup-summary-memorandum-display))
		       (y-or-n-p "Are you sure to remove this memorandum? ")))
	(lookup-entry-set-bookmark entry t)
	(lookup-module-add-bookmark (lookup-current-module) entry)
	(lookup-summary-update-mark)))))

(defvar lookup-summary-memorandum-entry nil)

(defun lookup-summary-memorandum ()
  (interactive)
  (let ((buffer (lookup-summary-memorandum-display)))
    (when buffer
      (select-window (get-buffer-window buffer))
      (message "Type `C-c C-c' to finish editing"))))

(defun lookup-summary-memorandum-display ()
  (let* ((entry (lookup-summary-this-entry))
	 (memo (lookup-entry-bookmark entry)))
    (with-current-buffer (lookup-open-buffer "*Entry Memorandum*")
      (erase-buffer)
      (text-mode)
      (make-variable-buffer-local 'lookup-summary-memorandum-entry)
      (setq lookup-summary-memorandum-entry entry)
      (local-set-key "\C-c\C-c" 'lookup-summary-memorandum-save)
      (insert (if (stringp memo) memo
		(funcall lookup-initial-memorandum entry)))
      (goto-char (point-min))
      (lookup-display-buffer (current-buffer)))))

(defun lookup-summary-memorandum-save ()
  (interactive)
  (let ((entry lookup-summary-memorandum-entry))
    (lookup-entry-set-bookmark entry (buffer-string))
    (lookup-module-add-bookmark (lookup-current-module) entry))
  (kill-buffer (current-buffer))
  (select-window (get-buffer-window (lookup-summary-buffer)))
  (lookup-summary-display-content)
  (lookup-summary-update-mark))

(defun lookup-summary-entry-open ()
  "エントリ本文を別プログラムで表示する。"
  (interactive)
  (unless (lookup-entry-open (lookup-summary-this-entry))
    (error "This entry doesn't have a open command")))

(defun lookup-summary-toggle-format ()
  "エントリ本文の整形処理をトグルする。"
  (interactive)
  (with-current-buffer (lookup-content-buffer)
    (lookup-content-toggle-format)))

(defun lookup-summary-toggle-example ()
  (interactive)
  (setq lookup-enable-example (not lookup-enable-example))
  (with-current-buffer (lookup-content-buffer)
    (let ((inhibit-read-only t))
      (lookup-adjust-content lookup-content-entry)))
  (message (if lookup-enable-example "Examples enabled" "Examples disabled")))

(defun lookup-summary-toggle-autobook ()
  (interactive)
  (setq lookup-summary-autobook-mode (not lookup-summary-autobook-mode))
  (force-mode-line-update))

(defun lookup-summary-toggle-overview ()
  "Overview モードに入る。
Cotent バッファがクローズされ、`n' と `p' が単にポイント移動だけを
行なうようになる。"
  (interactive)
  (if (setq lookup-summary-overview-mode (not lookup-summary-overview-mode))
      (lookup-hide-buffer (lookup-content-buffer))
    (lookup-display-buffer (lookup-content-buffer)))
  (force-mode-line-update))

(defun lookup-summary-isearch-content (&optional rexexp-p)
  "Content バッファで isearch-forward を実行する。"
  (interactive "P")
  (lookup-with-buffer-and-window (lookup-content-buffer)
    (isearch-forward rexexp-p)))

(defconst lookup-summary-default-policies
  '((asis . ((gaiji . glyph)))
    (plain . ((gaiji . alternate)))))

(defun lookup-summary-cite-content ()
  "エントリ本文をキルリングに保存する。
See also `lookup-content-cite-region'."
  (interactive)
  (unless (lookup-summary-content-visible-p)
    (lookup-summary-display-content))
  (with-current-buffer (lookup-content-buffer)
    (lookup-content-cite-region (point-max) (point-min)))
  (when (interactive-p)
    (message "Saved text for `%s'"
	     (lookup-entry-heading (lookup-summary-this-entry)))))

(defun lookup-summary-dictionary-menu ()
  "辞書がメニューに対応している場合、それを参照する。"
  (interactive)
  (let ((entry (lookup-summary-this-entry)))
    (when entry
      (let ((menu (lookup-dictionary-menu (lookup-entry-dictionary entry))))
	(if menu
	    (lookup-display-entries (lookup-current-module)
				    (lookup-new-query 'reference "Menu")
				    (list menu))
	  (error "This dictionary doesn't have a menu"))))))

(defun lookup-summary-list-references ()
  "エントリ本文に含まれるリファレンスを一覧する。"
  (interactive)
  (unless (lookup-summary-content-visible-p)
    (lookup-summary-display-content))
  (let ((entries (lookup-content-collect-references)))
    (if entries
	(let* ((heading (lookup-entry-heading (lookup-summary-this-entry)))
	       (query (lookup-new-query 'reference heading)))
	  (lookup-display-entries (lookup-current-module) query entries))
      (error "No valid reference in this entry"))))

(defun lookup-summary-content-window ()
  "Content バッファに移動する。"
  (interactive)
  (unless (lookup-summary-content-visible-p)
    (lookup-summary-display-content))
  (select-window (get-buffer-window (lookup-content-buffer))))

;;;
;;; Redo
;;;

(defun lookup-summary-redo (&optional module)
  "今回と同じ検索語で、エントリを再検索する。"
  (interactive (list (if current-prefix-arg (lookup-input-module))))
  (let ((query (lookup-session-query (lookup-current-session))))
    (if (not (eq (lookup-query-method query) 'reference))
	(let ((lookup-force-update t))
	  (lookup-search-session (or module (lookup-current-module)) query))
      (error "This session cannot be updated"))))

(defun lookup-summary-redo-exactly ()
  (interactive)
  (lookup-summary-redo-1 'exact))

(defun lookup-summary-redo-keyword ()
  (interactive)
  (lookup-summary-redo-1 'keyword))

(defun lookup-summary-redo-prefix ()
  (interactive)
  (lookup-summary-redo-1 'prefix))

(defun lookup-summary-redo-suffix ()
  (interactive)
  (lookup-summary-redo-1 'suffix))

(defun lookup-summary-redo-1 (method)
  (let* ((query (lookup-session-query (lookup-current-session)))
	 (string (lookup-query-string query)))
    (lookup-search-session (lookup-current-module)
			   (lookup-new-query method string))))

(defun lookup-summary-redo-nth-dictionary ()
  "Search again by using only the Nth dictionary in the current module.
This command should be binded to numerical keys (i.e., `1'..`9'),
which indicates the number of the dictionary."
  (interactive)
  (let* ((module (lookup-current-module))
	 (dict (nth (- (aref (this-command-keys) 0) ?1)
		    (lookup-module-dictionaries module)))
	 (lookup-valid-dictionaries (list dict))
	 (query (lookup-session-query (lookup-current-session))))
    (if dict
	(lookup-search-session module query)
      (error "No dictionary on the number: %s" (this-command-keys)))))

(defun lookup-summary-redo-all-dictionary ()
  "Search again by using all dictionary in the current module."
  (interactive)
  (let* ((module (lookup-current-module))
	 (lookup-valid-dictionaries (lookup-module-dictionaries module))
	 (query (lookup-session-query (lookup-current-session))))
    (lookup-search-session module query)))

(defun lookup-summary-update-content ()
  "エントリ本文を再表示する。整形処理も全てやり直される。"
  (interactive)
  (let ((lookup-force-update t))
    (lookup-summary-display-content)))

;;;;;;;;;;;;;;;;;;;;
;; Internal Functions
;;;;;;;;;;;;;;;;;;;;

(defun lookup-summary-goto-link ()
  (let ((p (progn (beginning-of-line) (point))))
    (if (setq p (next-single-property-change p 'lookup-entry))
	(goto-char p))))

(defun lookup-summary-this-entry ()
  (let ((entry (save-excursion
		 (end-of-line)
		 (if (not (eobp))
		     (get-text-property (1- (point)) 'lookup-entry)))))
    (when entry
      (if (not (eq (lookup-entry-type entry) 'dynamic))
	  (lookup-entry-substance entry)
	(lookup-summary-expand-references entry)
	(lookup-summary-this-entry)))))

(defun lookup-summary-update-mark ()
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (delete-char 1)
      (lookup-search-session-insert-mark (lookup-summary-this-entry)))))

(defun lookup-summary-content-visible-p ()
  (and (get-buffer-window (lookup-content-buffer))
       (eq (lookup-summary-this-entry) (lookup-content-entry))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lookup Content mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst lookup-content-mode-help
  "Lookup Content モード:

`SPC' - ページを進める          `<'   - バッファの最初へ
`DEL' - ページを戻る            `>'   - バッファの最後へ

`TAB' - 次のリンクへ            `RET' - リンクを辿る

`t'   - 整形処理をトグルする    `w'   - リージョンを引用
`h'   - Entry バッファに移動    `g'   - バッファを更新する
`q'   - バッファを抜ける        `?'   - ヘルプを表示")

(defvar lookup-content-mode-map nil
  "*Keymap for Lookup Content mode.")

(unless lookup-content-mode-map
  (setq lookup-content-mode-map (make-sparse-keymap))
  (set-keymap-parent lookup-content-mode-map lookup-global-map)
  (define-key lookup-content-mode-map " " 'scroll-up)
  (define-key lookup-content-mode-map "\C-?" 'scroll-down)
  (define-key lookup-content-mode-map [delete] 'scroll-down)
  (define-key lookup-content-mode-map "<" 'beginning-of-buffer)
  (define-key lookup-content-mode-map ">" 'end-of-buffer)
  (define-key lookup-content-mode-map "\C-i" 'lookup-content-next-link)
  (define-key lookup-content-mode-map "\C-m" 'lookup-content-follow-link)
  (define-key lookup-content-mode-map "t" 'lookup-content-toggle-format)
  (define-key lookup-content-mode-map "w" 'lookup-content-cite-region)
  (define-key lookup-content-mode-map "h" 'lookup-content-entry-window)
  (define-key lookup-content-mode-map "g" 'lookup-content-update)
  (define-key lookup-content-mode-map "q" 'lookup-content-leave)
  (define-key lookup-content-mode-map
    (if (featurep 'xemacs) 'button2 [mouse-2]) 'lookup-content-mouse-follow)
  )

(defvar lookup-content-mode-hook nil)

(defvar lookup-content-entry nil)
(defvar lookup-content-line-heading nil)

(make-variable-buffer-local 'lookup-content-entry)
(make-variable-buffer-local 'lookup-content-line-heading)

(defun lookup-content-mode ()
  "\\{lookup-content-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-content-mode)
  (setq mode-name "Content")
  (setq mode-line-buffer-identification
	'("Lookup:%b {" lookup-content-line-heading "}"))
  (setq lookup-mode-help lookup-content-mode-help)
  (setq buffer-read-only t)
  (make-variable-buffer-local 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (use-local-map lookup-content-mode-map)
  (run-hooks 'lookup-content-mode-hook))

;;;
;;; Interactive commands
;;;

(defun lookup-content-next-link ()
  "次のリンクに移動する。"
  (interactive)
  (if (lookup-goto-next-link)
      (message (lookup-entry-id (lookup-get-link (point))))
    (if (lookup-get-link (point))
	(error "No more link in this buffer")
      (goto-char (point-min))
      (if (lookup-goto-next-link)
	  (message (lookup-entry-id (lookup-get-link (point))))
	(error "No link in this buffer")))))

(defun lookup-content-follow-link ()
  "ポイント位置のリンクを参照する。"
  (interactive)
  (let ((entry (lookup-get-link (point))))
    (if entry
	(let ((entries (lookup-entry-substance entry)))
	  (if (setq entries (if entries
				(list entries)
			      (lookup-entry-references entry)))
	      (let* ((heading (lookup-entry-heading lookup-content-entry))
		     (query (lookup-new-query 'reference heading)))
		(lookup-display-entries (lookup-current-module) query entries))
	    (error "This link is torn off")))
      (error "No link here"))))

(defun lookup-content-mouse-follow (event)
  "マウスでクリックしたリンクを参照する。"
  (interactive "e")
  (mouse-set-point event)
  (lookup-content-follow-link))

(defun lookup-content-toggle-format ()
  "本文の整形処理をトグルする。"
  (interactive)
  (setq lookup-enable-format (not lookup-enable-format))
  (lookup-content-display lookup-content-entry))

(defun lookup-content-cite-region (start end)
  "リージョンの内容をキルリングに保存する。
その際、変数 `lookup-cite-header' または辞書オプション `cite-header'
により引用時のヘッダを、変数 `lookup-cite-prefix' または辞書オプション
`cite-prefix' により引用時のプレフィクスを指定することが出来る。"
  (interactive "r")
  (let* ((dictionary (lookup-entry-dictionary lookup-content-entry))
	 (header (or (lookup-dictionary-option dictionary ':cite-header t)
		     lookup-cite-header))
	 (prefix (or (lookup-dictionary-option dictionary ':cite-prefix t)
		     lookup-cite-prefix))
	 (contents (buffer-substring-no-properties start end)))
    (when prefix
      (with-temp-buffer
	(insert contents)
	(goto-char (point-min))
	(while (not (eobp))
	  (insert prefix)
	  (forward-line))
	(setq contents (buffer-string))))
    (when header
      (let ((title (lookup-dictionary-title dictionary)))
	(while (string-match "%T" header)
	  (setq header (replace-match title t t header))))
      (setq contents (concat header contents)))
    (kill-new contents)
    (if transient-mark-mode (setq deactivate-mark t))
    (when (interactive-p)
      (if (pos-visible-in-window-p (mark) (selected-window))
	  (let ((inhibit-quit t))
	    (save-excursion (goto-char (mark)) (sit-for 1)))
	(let ((len (min (abs (- end start)) 40)))
	  (if (= (point) start)
	      (message "Saved text until \"%s\""
		       (buffer-substring (- end len) end))
	    (message "Saved text from \"%s\""
		     (buffer-substring start (+ start len)))))))))

(defun lookup-content-entry-window ()
  "Entry バッファに移動する。"
  (interactive)
  (select-window (get-buffer-window (lookup-summary-buffer))))

(defun lookup-content-update ()
  "キャッシュを用いずに本文を読み直す。"
  (interactive)
  (let ((lookup-force-update t))
    (lookup-content-display lookup-content-entry)))

(defun lookup-content-leave ()
  "Content バッファを抜ける。"
  (interactive)
  (lookup-hide-buffer (current-buffer))
  (lookup-summary-display-content))

;;;
;;; Useful functions
;;;

(defun lookup-content-entry ()
  (with-current-buffer (lookup-content-buffer)
    lookup-content-entry))

(defun lookup-content-collect-references ()
  (with-current-buffer (lookup-content-buffer)
    (let (entries)
      (lookup-map-over-property
       (point-min) (point-max) 'lookup-reference
       (lambda (start end entry)
	 (setq entries (cons entry entries))))
      (nreverse entries))))

(provide 'lookup-search)

;;; lookup-search.el ends here
