;;; ndsrd.el --- search agent for 小学館『ランダムハウス英語辞典』 -*- lexical-binding: t -*-
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>
;; Copyright (C) 2009 Lookup Development Team

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Author: Taichi KAWABATA <kawabata.taichi@gmail.com>
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

;;; Documentation

;; If you want to display image, please specify "IMG" to be "@img" in 
;; your csrd.fmt file.
;; If you want to enable sound play, please install `realplayer' and 
;; in your csrd.fmt file, specify "SND" to be "@snd".

;;; Code:

(require 'cl-lib)
(require 'lookup)
(require 'lookup-content)
(require 'ndeb-binary)
(require 'hmac-md5)

;;;
;;; Customizable variables
;;;

(defgroup ndsrd nil
  "Lookup csrd interface."
  :group 'lookup-search-agents)

(defcustom ndsrd-program-name "csrd"
  "*Command name of `csrd'."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-dictionary-title "ランダムハウス英語辞典"
  "*Title of ndsrd dictionary."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-process-coding-system
  (if (memq system-type '(ms-dos windows-nt OS/2 emx))
      'cp932
    'euc-jp)
  "*Coding system for csrd process."
  :type 'symbol
  :group 'ndsrd)

;; Make sure that RealPlayer 11 or later is installed (Windows).
;; Macintosh RealPlayer can not play old RealAudio (`dnet') format, 
;; so you may need to install other player (such as `mplayer').
(defcustom ndsrd-sound-player 
  (cond ((equal system-type 'darwin)
         "mplayer")
        (t "realplayer"))
  "*Sound Player of ndsrd dictionary. 
It must be able to play `.ra' format (`dnet'-type) sound."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-temporary-directory
  (expand-file-name
   (concat temporary-file-directory "/ndsrd"))
  "*Temporary Directory for SRD Sound file."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-image-converter "convert - png:-"
  "shell command to convert compressed BMP to PNG.
It must convert the stdin stream to stdout stream."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-sound-header "@snd"
  "*Sound Data Indicator of ndsrd dictionary. 
It should be set by preference file specifid by `:fmt' option."
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-image-header "@img"
  "*Image Data Indicator of ndsrd dictionary.
It should be set by preference file specifid by `:fmt' option."
  :type 'string
  :group 'ndsrd)


;;;
;;; Internal Variables
;;;

(defvar ndsrd-temp-files nil)

(defvar ndsrd-link-map nil)
(unless ndsrd-link-map
  (setq ndsrd-link-map (copy-keymap lookup-content-mode-map))
  (define-key ndsrd-link-map "\C-m" 'ndsrd-follow-link)
  (define-key ndsrd-link-map "e" 'ndsrd-extract-link)
  (define-key ndsrd-link-map [mouse-2] 'ndsrd-mouse-follow))

;;;
;;; types
;;;

(put 'ndsrd :methods '(exact prefix suffix substring wildcard keyword))
(put 'ndsrd :reference-pattern '("→\\([A-Z]*\\)" 0 1 lookup-dynamic-search))
(put 'ndsrd :stemmer 'stem-english)
(put 'ndsrd :arranges '((structure ndsrd-arrange-structure)))


;;;
;;; Interface functions
;;;

(put 'ndsrd :list 'ndsrd-list)
(defun ndsrd-list (agent)
  (unless (file-exists-p (lookup-agent-location agent))
    (error "ndsrd: error. data file missing."))
  (unless (executable-find ndsrd-program-name)
    (error "ndsrd: error. program is missing."))
  (list (lookup-new-dictionary agent "")))

(put 'ndsrd :title ndsrd-dictionary-title)

(put 'ndsrd :search 'ndsrd-dictionary-search)
(defun ndsrd-dictionary-search (dictionary query)
  (with-current-buffer (get-buffer-create " *ndsrd*")
    (goto-char (point-max))
    (let ((opts (lookup-get-property dictionary 'ndsrd-opts)))
      (unless opts
	(let* ((agent (lookup-dictionary-agent dictionary))
	       (dir (expand-file-name (lookup-agent-location agent)))
	       (gai (lookup-agent-option agent :gai))
	       (fmt (lookup-agent-option agent :fmt)))
	  (setq opts '("-a"))
	  (if gai (setq opts (cons (concat "-g" (expand-file-name gai)) opts)))
	  (if fmt (setq opts (cons (concat "-f" (expand-file-name fmt)) opts)))
	  (setq opts (cons (concat "-d" dir) opts))
	  (lookup-put-property dictionary 'ndsrd-opts opts)))
      (setq opts (append opts (if (eq (lookup-query-method query) 'keyword)
				  (list "-i" (lookup-query-string query))
				(list (lookup-query-to-wildcard query)))))
      (if lookup-enable-debug
	  (insert "> " ndsrd-program-name " " (mapconcat 'eval opts " ") "\n"))
      (goto-char
       (prog1 (point)
	 (lookup-with-coding-system ndsrd-process-coding-system
	   (apply 'call-process ndsrd-program-name nil t nil opts)))))
    (let (start heading entry entries)
      (while (looking-at "□ \\([^\[\n]*\\)")
	(setq start (point) heading (match-string 1))
	(while (string-match "[・´｀/]\\| $" heading)
	  (setq heading (replace-match "" t t heading)))
	(forward-line)
	(if (re-search-forward "^□" nil 0)
	    (goto-char (match-beginning 0)))
	(setq entry (lookup-new-entry 'regular dictionary heading heading))
	(lookup-put-property entry 'ndsrd-content
			     (buffer-substring start (point)))
	(setq entries (cons entry entries)))
      (if (not lookup-enable-debug) (kill-buffer (current-buffer)))
      (nreverse entries))))

(put 'ndsrd :content 'ndsrd-entry-content)
(defun ndsrd-entry-content (entry)
  (or (lookup-get-property entry 'ndsrd-content) "(forgot)"))

(put 'ndsrd :kill 'ndsrd-kill)
(defun ndsrd-kill (_agent)
  (mapc (lambda (x)
          (if (file-exists-p x) (delete-file x)))
        ndsrd-temp-files)
  (setq ndsrd-temp-files nil)
  (if (and (file-directory-p ndsrd-temporary-directory)
           (null (cl-set-difference 
             (directory-files ndsrd-temporary-directory)
             '("." "..") :test 'equal)))
      (delete-directory ndsrd-temporary-directory)))

;;;
;;; Sound and Image Processing
;;;

(defun ndsrd-image-file (agent)
  (let ((location (lookup-agent-location agent)))
    (expand-file-name
     (concat location "/img.dat"))))

(defun ndsrd-sound-file (agent)
  (let ((location (lookup-agent-location agent)))
    (expand-file-name
     (concat location "/srdra.bnd"))))

(defun ndsrd-arrange-structure (entry)
  "Arrange sound and image portion of ENTRY."
  (interactive)
  (let ((agent (lookup-dictionary-agent 
                (lookup-entry-dictionary entry)))
        file offset size start end)
    (while (re-search-forward 
            (concat ndsrd-sound-header 
                    "\\([0-9a-f]+\\),\\([0-9a-f]+\\)\n")
            nil t)
      (setq file   (ndsrd-sound-file agent)
            offset (string-to-number (match-string 1) 16)
            size   (string-to-number (match-string 2) 16)
            start  (match-beginning 0)
            end    (match-end 0))
      (ndsrd-sound-set-link start end file offset size))
    (goto-char (point-min))
    (while (re-search-forward 
            (concat ndsrd-image-header 
                    "\\([0-9a-f]+\\),\\([0-9a-f]+\\)$")
            nil t)
      (setq file   (ndsrd-image-file agent)
            offset (string-to-number (match-string 1) 16)
            size   (string-to-number (match-string 2) 16)
            start  (match-beginning 0)
            end    (match-end 0))
      (ndsrd-insert-image start end file offset size))))

(defun ndsrd-sound-set-link (start end file offset size)
  (save-restriction
    (narrow-to-region start end)
    (delete-region (point-min) (point-max))
    (insert "《音声》")
    (add-text-properties 
     (point-min) (point-max)
     (list 'keymap ndsrd-link-map
           'face 'ndeb-sound-caption-face
           'mouse-face 'highlight
           'help-echo "[sound] mouse-2: play, e:extract"
           'lookup-tab-stop t
           'ndsrd-sound (list file offset size)))))

(defun ndsrd-insert-image (start end file offset size)
  (let ((image
         (create-image
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally
             file nil offset (+ size offset))
            (shell-command-on-region 
             (point-min) (point-max)
             ndsrd-image-converter
             nil t "*Messages*" lookup-enable-debug)
            (buffer-string))
          'png t)))
    (save-restriction
      (narrow-to-region start end)
      (delete-region start end)
      (insert-image image "《画像》"))))

(defun ndsrd-create-temporary-file (directory key extension)
  "Create unique temp-file name for DIRECTORY, KEY, and EXTENSION.
DATA may be any valid lisp expression."
  (let ((temp-file-name
         (expand-file-name
          (concat
           directory "/"
           (encode-hex-string
            (hmac-md5 (format "%s" key) (make-string 16 ?\x0b)))
           extension))))
    (unless (file-directory-p ndsrd-temporary-directory)
      (make-directory ndsrd-temporary-directory t)
      (set-file-modes ndsrd-temporary-directory 448))
    (unless (file-exists-p temp-file-name)
      (setq ndsrd-temp-files (cons temp-file-name
                                   ndsrd-temp-files)))
    temp-file-name))

(defun ndsrd-save-temporary-file (file offset size temp-file)
  "Save contents of FILE of OFFSET, SIZE to TEMP-FILE."
  (with-temp-file temp-file
    (insert-file-contents-literally
     file nil offset (+ size offset))))

(defun ndsrd-mouse-follow (event)
  (interactive "e")
  (mouse-set-point event)
  (ndsrd-follow-link))

(defun ndsrd-follow-link ()
  "Play the sound at point."
  (interactive)
  (let ((data (get-text-property (point) 'ndsrd-sound)))
    (when data
      (let* ((file (elt data 0))
             (offset (elt data 1))
             (size (elt data 2))
             (temp-file
              (ndsrd-create-temporary-file
               ndsrd-temporary-directory (list file offset size) ".ra")))
        (unless (file-exists-p temp-file)
          (ndsrd-save-temporary-file file offset size temp-file))
        (call-process ndsrd-sound-player nil nil nil temp-file)))))

(defun ndsrd-extract-link ()
  "Extract the sound at point"
  (interactive)
  (let ((data (get-text-property (point) 'ndsrd-sound)))
    (when data
      (let* ((file (elt data 0))
             (offset (elt data 1))
             (size (elt data 2)))
        (with-temp-file
            (read-file-name (format "Save realaudio into file: "))
          (insert-file-contents-literally
           file nil offset (+ offset size)))))))

(provide 'ndsrd)

;;; ndsrd.el ends here
