;;; srd-fpw.el --- supplement file for 『ランダムハウス英語辞典』

;; Copyright (C) 2000 Keisuke Nishida <kxn30@po.cwru.edu>
;; Copyright (C) 2000 Kazuhiko Shiozaki <kazuhiko@ring.gr.jp>
;; Copyright (C) 2000 Kazuyoshi KOREEDA <k_koreed@d2.dion.ne.jp>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)
(require 'poem)

(defvar srd-fpw-data-directory "/usr/local/dict/srd"
  "img.dat, srdra.bnd のある場所。")
(defvar play-realaudio-process "realplay"
  "RealAudio を再生するプロセス名。nil なら再生しない。")
(defvar display-image-process "display"
  "画像 を表示するプロセス名。nil なら表示しない。")
(defvar srd-fpw-sound-without-notice nil
  "t なら検索と同時に音声を再生する。")
(defvar display-image-inline t
  "nil なら (可能な場合でも) 画像をインライン表示しない。")
(defvar perl-process "perl"
  "perl のプロセス名。パスが通っていない場合はフルパスで記述すること。")

(defvar srd-fpw-tmp-dir temporary-file-directory "一時ファイルの作成場所")

(defvar srd-fpw-process-file-alist '())

;; 
;; 元からある lookup-content-follow-linkの拡張
;; 
; (unless (fboundp 'lookup-content-follow-link:old)
;   (fset 'lookup-content-follow-link:old
; 	  (symbol-function 'lookup-content-follow-link))
;   (defun lookup-content-follow-link ()
;     (interactive)
;     (let ((action (get-text-property (point) 'action)))
; 	(if action 
; 	    (funcall action (point))
; 	  (lookup-content-follow-link:old)))))

(defun srd-fpw-arrange-structure (entry)
  (srd-fpw-arrange-images entry)
  (goto-char (point-min))
  (srd-fpw-arrange-realaudio entry))

(defun srd-fpw-arrange-images (entry)
  (while (re-search-forward
	  "<image=\\([^:]+\\):\\([^>]+\\)>" nil t)
    (let ((file "img.dat")
	  (offset (match-string 1))
	  (length (match-string 2))
	  (start (match-beginning 0))
	  (end (match-end 0)))
      ;; Find data file.
      (if (file-exists-p (expand-file-name file srd-fpw-data-directory))
	  (setq file (expand-file-name file srd-fpw-data-directory)))
      (progn
	(replace-match "→[画像]")
	(add-text-properties start 
			     (+ (length "→[画像]") start)
			     (list 'action 'srd-fpw-display-image
				   'file  file
				   'offset offset
				   'mouse-face 'highlight
				   'face 'lookup-reference-face
				   'length   length))))))

(defun srd-fpw-arrange-realaudio (entry)
  (while (re-search-forward
	  "<sound=\\([^:]+\\):\\([^>]+\\)>" nil t)
    (let ((file "srdra.bnd")
	  (offset (match-string 1))
	  (length (match-string 2))
	  (start (match-beginning 0))
	  (end (match-end 0)))
      ;; Find data file.
      (if (file-exists-p (expand-file-name file srd-fpw-data-directory))
	  (setq file (expand-file-name file srd-fpw-data-directory)))
      (if srd-fpw-sound-without-notice
	  (let* ((tmp-snd-file
		  (make-temp-name 
		   (expand-file-name "sr" temporary-file-directory)))
		 )
	    (if play-realaudio-process
		(progn
		  (call-process
		   perl-process nil nil nil
		   (expand-file-name "extract.pl" lookup-support-directory)
		   file offset length tmp-snd-file)
		  (srd-fpw-start-process play-realaudio-process
					 nil tmp-snd-file t)))))
      (replace-match "→[音声]")
      (add-text-properties start 
			   (+ (length "→[音声]") start)
			   (list 'action 'srd-fpw-play-realaudio
				 'file  file
				 'offset offset
				 'mouse-face 'highlight
				 'face 'lookup-reference-face
				 'length   length)))))
;;
;; 外部プロセスを利用したイメージの表示
;;
(defun srd-fpw-display-image (pos)
  (let* ((file (get-text-property pos 'file))
	 (offset (get-text-property pos 'offset))
	 (length (get-text-property pos 'length))
	 (tmp-img-file (make-temp-name 
			(expand-file-name "sr" temporary-file-directory))))
    (if display-image-process
	(progn
	  (call-process
	   perl-process nil nil nil
	   (expand-file-name "extract.pl" lookup-package-directory)
	   file offset length tmp-img-file)
	  (srd-fpw-start-process display-image-process nil tmp-img-file t)))))
;;
;; 外部プロセスを利用した音声の再生
;;
(defun srd-fpw-play-realaudio (pos)
  (let* ((file (get-text-property pos 'file))
	 (offset (get-text-property pos 'offset))
	 (length (get-text-property pos 'length))
	 (tmp-snd-file (make-temp-name 
			(expand-file-name "sr" temporary-file-directory)))
	 )
    (if play-realaudio-process
	(progn
	  (call-process
	   perl-process nil nil nil
	   (expand-file-name "extract.pl" lookup-package-directory)
	   file offset length tmp-snd-file)
	  (srd-fpw-start-process play-realaudio-process nil tmp-snd-file t)))))
;;
;; 外部プロセスの呼出し
;; 
(defun srd-fpw-start-process (program options file &optional delete-file)
  (message "Starting %s ..." program)
  (let ((pro (apply (function start-process)
		    (format "*srd-fpw %s*" program)
		    nil
		    "ssh"
		    (append (list "kei" program)
			    options (list (concat "/mnt/indy" file))))))
    (message "Starting %s ... done" program)
    (set-process-sentinel pro 'srd-fpw-start-process-sentinel)
    (setq srd-fpw-process-file-alist 
	  (cons (cons pro file) 
		(if delete-file 
		    srd-fpw-process-file-alist
		  nil)))))
;;
;; プロセスの状態が変更されたときにファイルを削除する。
;;
(defun srd-fpw-start-process-sentinel (process event)
  (let ((al (assoc process srd-fpw-process-file-alist)))
    (and (cdr al) (delete-file (cdr al)))
    (setq srd-fpw-process-file-alist
	  (delete al srd-fpw-process-file-alist))))

(setq lookup-support-options
      (list :arranges '((structure srd-fpw-arrange-structure))))

;;; srd-fpw.el ends here
