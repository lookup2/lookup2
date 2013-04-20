;;; lookup-content.el --- Lookup Content mode -*- lexical-binding: t -*-
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>
;; Copyright (C) 2009 Lookup Development Team

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

(require 'lookup-types)
(declare-function lookup-get-link "lookup")
(declare-function lookup-goto-next-link "lookup")
(declare-function lookup-goto-previous-link "lookup")
(declare-function lookup-display-entries "lookup")
(declare-function lookup-current-module "lookup")
(declare-function lookup-summary-buffer "lookup")
(declare-function lookup-hide-buffer "lookup")
(declare-function lookup-content-buffer "lookup")
(declare-function lookup-content-display "lookup-summary")
(declare-function lookup-summary-display-content "lookup-summary")

(defconst lookup-content-mode-help
  "Lookup Content Mode:

`SPC' - Next page               `<'   - Beginning of buffer
`DEL' - Previous page           `>'   - End of buffer

`TAB' - Next link               `RET' - Jump to link

`t'   - Toggle arrangements     `w'   - Yank region
`h'   - Move to entris buffer   `g'   - Update buffer
`q'   - Quit buffer             `?'   - Display help")

(defvar lookup-content-mode-map nil
  "*Keymap for Lookup Content mode.")

(when (< (length lookup-content-mode-map) 2)
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
  (define-key lookup-content-mode-map [mouse-2] 'lookup-content-mouse-follow))

(defvar lookup-content-mode-hook nil)

(defvar lookup-content-entry nil)
(defvar lookup-content-line-heading nil)

(make-variable-buffer-local 'lookup-content-entry)
(make-variable-buffer-local 'lookup-content-line-heading)
(make-variable-buffer-local 'line-move-ignore-invisible)

;;;###autoload
(defun lookup-content-mode ()
  "\\{lookup-content-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-content-mode)
  (setq mode-name "Content")
  (setq mode-line-buffer-identification
        '("Lookup:%b {" lookup-content-line-heading "}"))
  (setq lookup-help-message lookup-content-mode-help)
  (setq buffer-read-only t)
  (setq line-move-ignore-invisible t)
  (use-local-map lookup-content-mode-map)
  (run-hooks 'lookup-content-mode-hook))

;;;
;;; Interactive commands
;;;

(defun lookup-content-next-link ()
  "現在または直近のリンクに移動する。"
  (interactive)
  (or (lookup-get-link (point))
      (if (lookup-goto-next-link)
          (message (lookup-entry-id (lookup-get-link (point))))
        (if (lookup-get-link (point))
            (error "No more link in this buffer")
          (goto-char (point-min))
          (if (lookup-goto-next-link)
              (message (lookup-entry-id (lookup-get-link (point))))
            (error "No link in this buffer"))))))

(defun lookup-content-previous-link ()
  (interactive)
  (or (lookup-get-link (point))
      (if (lookup-goto-previous-link)
          (message (lookup-entry-id (lookup-get-link (point))))
        (error "No more link in this buffer"))))

(defun lookup-content-follow-link ()
  "ポイント位置のリンクを参照する。"
  (interactive)
  (let ((entry (lookup-get-link (point))))
    (if (and entry (equal 'url (lookup-entry-type entry)))
        (browse-url (lookup-entry-code entry))
      (if entry
          (let ((entries (lookup-entry-substance entry)))
            (if (setq entries (if entries
                                  (list entries)
                                (lookup-entry-references entry)))
                (let* ((heading (lookup-entry-heading lookup-content-entry))
                       (query (lookup-new-query 'reference heading)))
                  (lookup-display-entries (lookup-current-module) query entries))
              (error "This link is torn off")))
        (error "No link here")))))

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
         (header (or (lookup-dictionary-option dictionary :cite-header t)
                     lookup-cite-header))
         (prefix (or (lookup-dictionary-option dictionary :cite-prefix t)
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
    (when (called-interactively-p 'interactive)
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
       (lambda (_start _end entry)
         (setq entries (cons entry entries))))
      (nreverse entries))))

(provide 'lookup-content)

;;; lookup-content.el ends here
