;;; install.el --- Lookup installer
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

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

(if (string< emacs-version "19.29")
    (setq command-line-args-left (cdr command-line-args-left)))

(or (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest body)
      (` (save-current-buffer
	   (set-buffer (, buffer))
	   (,@ body)))))

(defvar install-package-version nil)
(defvar install-elisp-files nil)
(defvar install-info-files nil)

(defun install-get-field (tag)
  (goto-char (point-min))
  (let (values)
    (if (re-search-forward (concat "^" tag " =") nil t)
	(progn
	  (while (looking-at "\\([ \t]\\|\\\\\n\\)*\\([^ \t\n\\]+\\)")
	    (setq values
		  (cons (buffer-substring (match-beginning 2) (match-end 2))
			values))
	    (goto-char (match-end 0)))
	  (nreverse values)))))

(with-current-buffer (generate-new-buffer " *lookup-temp*")
  (insert-file-contents "Makefile")
  (setq install-package-version (car (install-get-field "AUTHOR_VERSION")))
  (setq install-elisp-files (install-get-field "ELCS"))
  (setq install-info-files (install-get-field "INFO_FILES"))
  (kill-buffer (current-buffer)))

(defvar install-lisp-directory nil)
(defvar install-info-directory nil)

(defun install-check-directory (directory)
  (and (not (file-exists-p directory))
       (y-or-n-p (format "Directory %s is not exist.  Creat it? " directory))
       (make-directory directory t))
  (if (not (file-directory-p directory))
      (error "%s is not directory" directory))
  (directory-file-name (expand-file-name directory)))

;; message

(if noninteractive nil
  (switch-to-buffer (generate-new-buffer "*Lookup Installer*"))
  (insert "Lookup インストーラ\n")
  (insert "===================\n\n")
  (insert "Lookup のインストールを始めます。")
  (insert "途中で中断するには C-g を押して下さい。\n\n"))

;; directory

(if noninteractive nil
  (insert "ディレクトリの決定\n")
  (insert "------------------\n\n")
  (insert "elisp ファイルのディレクトリを入力して下さい:\n"))
(let ((default "~/emacs/lisp/lookup/"))
  (setq install-lisp-directory
	(install-check-directory
	 (if noninteractive
	     (or (car command-line-args-left) default)
	   (read-file-name "Lisp directory: " default default)))))
(if noninteractive nil
  (insert "    -> " install-lisp-directory "\n\n") (sit-for 0))

(if noninteractive nil
  (insert "info ファイルのディレクトリを入力して下さい:\n"))
(let ((default "~/emacs/info/"))
  (setq install-info-directory
	(install-check-directory
	 (if noninteractive
	     (or (car (cdr command-line-args-left)) default)
	   (read-file-name "Info directory: " default default)))))
(if noninteractive nil
  (insert "    -> " install-info-directory "\n\n") (sit-for 0))

;; lookup-vars.el

(if noninteractive nil
  (insert "インストールの実行\n")
  (insert "------------------\n\n")
  (insert "lookup-vars.el.in から lookup-vars.el を生成中...") (sit-for 0))
(with-current-buffer (find-file-noselect "lisp/lookup-vars.el.in" t)
  (if (search-forward "@VERSION@" nil t)
      (replace-match install-lookup-version))
  (write-file "lookup-vars.el")
  (kill-buffer (current-buffer)))
(message "Copied lookup-vars.el.in to lookup-vars.el")
(if (not noninteractive) (insert "done\n"))

;; lookup-autoloads.el
(if noninteractive nil
  (insert "lookup-autoloads.el の生成中...") (sit-for 0))
(with-current-buffer (generate-new-buffer " *lookup-temp*")
  (insert ";;; lookup-autoloads.el --- definition of autoloads for Lookup\n")
  (insert ";; This file automatically generated on "
	  (format-time-string "%B %e, %Y") " by install.el\n\n")
  (require 'autoload)
  (mapcar (lambda (generated-autoload-file)
	    (generate-file-autoloads
	     (expand-file-name generated-autoload-file "lisp/")))
	  (directory-files "lisp/" nil "\\.el\\'"))
  (insert "\n;;; lookup-autoloads.el ends here\n")
  (set-buffer-file-coding-system 'junet-unix)
  (write-file "lisp/lookup-autoloads.el")
  (kill-buffer (current-buffer)))
(if (not noninteractive) (insert "done\n"))

;; compile

(if noninteractive nil
  (insert "elisp ファイルのコンパイル中...") (sit-for 0))
(save-current-buffer
  (let ((load-path (cons nil load-path))
	(byte-compile-warnings nil)
	(lookup-byte-compiling t))
    (mapcar 'load install-elisp-files)
    (mapcar 'byte-compile-file install-elisp-files)))
(if (not noninteractive) (insert "done\n"))

;; install

(if noninteractive nil
  (insert "elisp ファイルのインストール中...") (sit-for 0))
(mapcar (lambda (file)
	  (setq file (file-name-nondirectory file))
	  (copy-file (expand-file-name file "lisp/")
		     (expand-file-name file install-lisp-directory) t)
	  (message "Installed %s to %s" file install-lisp-directory)
	  (setq file (byte-compile-dest-file file))
	  (copy-file (expand-file-name file "lisp/")
		     (expand-file-name file install-lisp-directory) t)
	  (message "Installed %s to %s" file install-lisp-directory))
	install-elisp-files)
(if (not noninteractive) (insert "done\n"))

(if noninteractive nil
  (insert "info ファイルのフォーマット中...") (sit-for 0))
(mapcar (lambda (info)
	  (if (file-readable-p (expand-file-name info "texi/"))
	      t
	    ;; the pre-formatted .info file does not exist; format it here
	    (let ((texi (concat (file-name-sans-extension info) ".texi")))
	      (save-current-buffer
		      (let ((buf (find-file-noselect 
				  (expand-file-name texi "texi/") t)))
			(set-buffer buf)
			(texinfo-format-buffer t) ; t for nosplit
			(save-buffer 0)
			(kill-buffer (current-buffer))
			(kill-buffer buf))))))
	install-info-files)
(if (not noninteractive) (insert "done\n"))

(if noninteractive nil
  (insert "info ファイルのインストール中...") (sit-for 0))
(mapcar (lambda (info)
	  (mapcar (lambda (file)
		    (copy-file (expand-file-name file "texi/")
			       (expand-file-name file install-info-directory)
			       t)
		    (message "Installed %s to %s" file install-info-directory))
		  (directory-files "texi/" nil info)))
	install-info-files)
(if (not noninteractive) (insert "done\n"))

;; clean

(if noninteractive nil
  (insert "一時ファイルの削除中...") (sit-for 0))
(delete-file "lisp/lookup-vars.el")
(mapcar (lambda (file)
	  (delete-file (byte-compile-dest-file (expand-file-name file))))
	install-elisp-files)
(message "Removed lookup-vars.el, *.elc")
(if (not noninteractive) (insert "done\n"))

;; initialize

(if noninteractive nil
  (insert "Lookup の初期化中...")
  (sit-for 0)
  (lookup-initialize)
  (insert "done\n")
  (kill-buffer (current-buffer)))

;; congratulate

(if noninteractive
    (message "\nSee etc/SETUP for the setup after installation")
  (switch-to-buffer (generate-new-buffer "*Congratulations!*"))
  (insert-file-contents "etc/SETUP")
  (if (search-forward "@install-lisp-directory@")
      (replace-match install-lisp-directory))
  (if (search-forward "@install-info-directory@")
      (replace-match install-info-directory))
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (view-mode)
  (local-set-key "\C-c\C-c" 'install-open-info))

(defun install-open-info ()
  (interactive)
  (require 'info)
  (Info-find-node (expand-file-name "lookup-guide" install-info-directory)
		  "Top"))

;;; install.el ends here
