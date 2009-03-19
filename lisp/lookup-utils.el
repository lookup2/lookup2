;;; lookup-utils.el --- Lookup various utilities
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

(require 'cl)
(require 'lookup-vars)

;; alist by assq

(defsubst lookup-assq-get (alist key)
  (cdr (assq key alist)))

(defsubst lookup-assq-del (alist key)
  (delq (assq key alist) alist))

(defun lookup-assq-put (alist key value)
  (if value
      (acons key value (lookup-assq-del alist key))
    (lookup-assq-del alist key)))

;; alist by assoc

(defsubst lookup-assoc-get (alist key)
  (cdr (assoc key alist)))

(defsubst lookup-assoc-del (alist key)
  (delq (assoc key alist) alist))

(defun lookup-assoc-put (alist key value)
  (if value
      (acons key value (lookup-assoc-del alist key))
    (lookup-assoc-del alist key)))

;; alist set/ref

(defsubst lookup-assq-ref (symbol key)
  (lookup-assq-get (symbol-value symbol) key))

(defsubst lookup-assq-set (symbol key value)
  (set symbol (lookup-assq-put (symbol-value symbol) key value)))

(defsubst lookup-assoc-ref (symbol key)
  (lookup-assoc-get (symbol-value symbol) key))

(defsubst lookup-assoc-set (symbol key value)
  (set symbol (lookup-assoc-put (symbol-value symbol) key value)))

;; multi put/get

(defsubst lookup-multi-get (symbol &rest args)
  (lookup-multi-get-1 (symbol-value symbol) args))

(defun lookup-multi-get-1 (alist args)
  (if args
      (lookup-multi-get-1 (lookup-assq-get alist (car args)) (cdr args))
    alist))

(defsubst lookup-multi-put (symbol &rest args)
  (set symbol (lookup-multi-put-1 (symbol-value symbol) args)))

(defun lookup-multi-put-1 (alist args)
  (if (cddr args)
      (lookup-assq-put alist (car args)
		       (lookup-multi-put-1 (lookup-assq-get alist (car args))
					   (cdr args)))
    (lookup-assq-put alist (car args) (cadr args))))

;; misc

(defun lookup-grep (predicate list)
  (let ((value nil))
    (while list
      (if (funcall predicate (car list))
	  (setq value (cons (car list) value)))
      (setq list (cdr list)))
    (nreverse value)))

(defun lookup-map-over-property (from to prop func &optional object)
  (let ((start from) end value)
    (while (setq value (get-text-property start prop object)
		 end (text-property-not-all start to prop value object))
      (if value (funcall func start end value))
      (setq start end))
    (if value (funcall func start to value))))

(defun lookup-current-line ()
  (save-excursion
    (do ((line 1 (1+ line)))
	((bobp) line)
      (forward-line -1))))

(defun lookup-reverse-string (string)
  (concat (nreverse (string-to-list string))))

(defun lookup-oneline-string (string)
  (while (string-match "\n *" string)
    (setq string (replace-match " " t t string)))
  string)

(defun lookup-read-string (prompt &optional init history default inherit)
  (read-string (if default
		   (concat prompt " (default " default "): ")
		 (concat prompt ": "))
	       init history default inherit))

(put 'lookup-with-coding-system 'lisp-indent-function 1)
(defmacro lookup-with-coding-system (coding &rest body)
  `(let ((coding-system-for-read ,coding)
	 (coding-system-for-write ,coding))
     ,@body))

(put 'lookup-with-buffer-and-window 'lisp-indent-function 1)
(defmacro lookup-with-buffer-and-window (buffer &rest body)
  `(let ((buffer ,buffer))
     (with-current-buffer buffer
       (save-selected-window
	 (if (get-buffer-window buffer)
	     (select-window (get-buffer-window buffer))
	   (error "No window for buffer `%s'" buffer))
	 ,@body))))

(defun lookup-table-insert (format args-list)
  (let ((format-list nil) (width-alist nil)
	(n 0) (start 0) (end 0) width)
    ;; parse format string
    (while (string-match "%\\(-?[0-9]*\\)." format start)
      (unless (eq (aref format (match-end 1)) ?%)
	(when (eq (aref format (match-end 1)) ?t)
	  (setq width (string-to-number (match-string 1 format)))
	  (lookup-assq-set 'width-alist n (cons width (abs width)))
	  (setq format-list
		(cons n (cons (substring format end (match-beginning 0))
			      format-list))
		end (match-end 0)))
	(setq n (1+ n)))
      (setq start (match-end 0)))
    (setq format-list (nreverse (cons (substring format end) format-list)))
    ;; get max width
    (dolist (args args-list)
      (dolist (pair width-alist)
	(setq width (string-width (nth (car pair) args)))
	(if (< (cddr pair) width)
	    (setcdr (cdr pair) width))))
    ;; construct real format
    (setq format (mapconcat
		  (lambda (element)
		    (if (stringp element)
			element
		      (let* ((pair (lookup-assq-ref 'width-alist element))
			     (string (if (> (car pair) 0)
					 (number-to-string (cdr pair))
				       (number-to-string (- (cdr pair))))))
			(concat "%" string "s"))))
		  format-list ""))
    ;; insert table
    (dolist (args args-list)
      (setq start (point))
      (insert (apply 'format format args))
      (put-text-property start (1- (point)) 'lookup args))))

(defvar lookup-property-table nil)

(defun lookup-get-property (obj key)
  (lookup-multi-get 'lookup-property-table obj key))

(defun lookup-put-property (obj key val)
  (lookup-multi-put 'lookup-property-table obj key val))

;;;
;;; Lookup current-word
;;;

(defun lookup-current-word ()
  (save-excursion
    (unless (eq (char-syntax (or (char-after (point)) 0)) ?w)
      (skip-syntax-backward "^w" (line-beginning-position))
      (if (bolp)
          (skip-syntax-forward "^w" (line-end-position))
        (backward-char)))
    (let* ((ch (or (char-after (point)) 0))
	   (charset (char-charset ch)))
      (cond ((and (eq charset 'japanese-jisx0208)
                  (< #x3000 ch))
             (lookup-current-word-japanese))
	    (t (lookup-current-word-general))))))

(defun lookup-current-word-general ()
  (if (fboundp 'thing-at-point)
      (thing-at-point 'word)
    (buffer-substring-no-properties
     (progn (skip-syntax-backward "w") (point))
     (progn (skip-syntax-forward "w") (point)))))

(defun lookup-current-word-japanese ()
  (if (not lookup-use-kakasi)
      (lookup-current-word-general)
    (let ((temp-buffer (lookup-temp-buffer))
	  (start (point)) (n 1) regexp)
      (lookup-with-coding-system lookup-kakasi-coding-system
	(call-process-region
	 (progn (skip-syntax-backward "w") (point))
	 (progn (skip-syntax-forward "w") (point))
	 lookup-kakasi-program nil temp-buffer nil "-w"))
      (with-current-buffer temp-buffer
	(goto-char (point-min))
	(while (search-forward " " nil t)
	  (replace-match "\\)\\(" nil t))
	(setq regexp (concat "\\(" (buffer-string) "\\)"))
	(kill-buffer (current-buffer)))
      (re-search-backward regexp)
      (while (and (match-end n) (<= (match-end n) start))
	(setq n (1+ n)))
      (buffer-substring-no-properties (match-beginning n) (match-end n)))))

;;;
;;; Lookup process
;;;

(defvar lookup-process-output-start nil)
(defvar lookup-process-output-value nil)
(defvar lookup-process-output-filter nil)
(defvar lookup-process-output-finished nil)
(defvar lookup-process-output-separator nil)
(defvar lookup-process-output-separator-lines 2)

(defun lookup-process-require (process string separator &optional filter)
  (setq lookup-process-output-value nil)
  (setq lookup-process-output-filter filter)
  (setq lookup-process-output-finished nil)
  (setq lookup-process-output-separator separator)
  (let (temp-buffer)
    (unless (process-buffer process)
      (setq temp-buffer (lookup-temp-buffer))
      (set-process-buffer process temp-buffer))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert string)
      (setq lookup-process-output-start (point))
      (set-process-filter process 'lookup-process-filter)
      (process-send-string process string)
      (while (not lookup-process-output-finished)
	(unless (accept-process-output process 5)
	  (when (> (point) lookup-process-output-start)
	    (display-buffer (current-buffer))
	    (error "Lookup BUG!! Report bug to the mailing list")))))
    (when temp-buffer
      (set-process-buffer process nil)
      (kill-buffer temp-buffer)))
  lookup-process-output-value)

(defun lookup-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (insert string)
    (forward-line (- lookup-process-output-separator-lines))
    (if (< (point) lookup-process-output-start)
	(goto-char lookup-process-output-start))
    (when (re-search-forward lookup-process-output-separator nil 0)
      (goto-char (match-beginning 0))
      (if lookup-process-output-filter
	  (save-current-buffer
	    (narrow-to-region lookup-process-output-start (point))
	    (goto-char (point-min))
	    (setq lookup-process-output-value
		  (funcall lookup-process-output-filter process))
	    (widen))
	(setq lookup-process-output-value
	      (buffer-substring lookup-process-output-start (point))))
      (setq lookup-process-output-finished t))))

(defun lookup-process-kill (process)
  (set-process-filter process nil)
  (delete-process process)
  (if (process-buffer process)
      (kill-buffer (process-buffer process))))

;;; Lookup text utilities

(defconst lookup-superscript-char-table
  '((?2 . ?²) (?3 . ?³) (?1 . ?¹)
    (?o . ?º) (?h . ?ʰ) (?ɦ . ?ʱ) (?j . ?ʲ)
    (?r . ?ʳ) (?ɹ . ?ʴ) (?ɻ . ?ʵ) (?ʁ . ?ʶ)
    (?w . ?ʷ) (?y . ?ʸ) (?ɣ . ?ˠ) (?l . ?ˡ)
    (?s . ?ˢ) (?x . ?ˣ) (?ʕ . ?ˤ) (?ნ . ?ჼ)
    (?A . ?ᴬ) (?Æ . ?ᴭ) (?B . ?ᴮ) (?D . ?ᴰ)
    (?E . ?ᴱ) (?Ǝ . ?ᴲ) (?G . ?ᴳ) (?H . ?ᴴ)
    (?I . ?ᴵ) (?J . ?ᴶ) (?K . ?ᴷ) (?L . ?ᴸ)
    (?M . ?ᴹ) (?N . ?ᴺ) (?O . ?ᴼ) (?Ȣ . ?ᴽ)
    (?P . ?ᴾ) (?R . ?ᴿ) (?T . ?ᵀ) (?U . ?ᵁ)
    (?W . ?ᵂ) (?a . ?ᵃ) (?ɐ . ?ᵄ) (?ɑ . ?ᵅ)
    (?ᴂ . ?ᵆ) (?b . ?ᵇ) (?d . ?ᵈ) (?e . ?ᵉ)
    (?ə . ?ᵊ) (?ɛ . ?ᵋ) (?ɜ . ?ᵌ) (?g . ?ᵍ)
    (?k . ?ᵏ) (?m . ?ᵐ) (?ŋ . ?ᵑ) (?o . ?ᵒ)
    (?ɔ . ?ᵓ) (?ᴖ . ?ᵔ) (?ᴗ . ?ᵕ) (?p . ?ᵖ)
    (?t . ?ᵗ) (?u . ?ᵘ) (?ᴝ . ?ᵙ) (?ɯ . ?ᵚ)
    (?v . ?ᵛ) (?ᴥ . ?ᵜ) (?β . ?ᵝ) (?γ . ?ᵞ)
    (?δ . ?ᵟ) (?φ . ?ᵠ) (?χ . ?ᵡ) (?н . ?ᵸ)
    (?ɒ . ?ᶛ) (?c . ?ᶜ) (?ɕ . ?ᶝ) (?ð . ?ᶞ)
    (?ɜ . ?ᶟ) (?f . ?ᶠ) (?ɟ . ?ᶡ) (?ɡ . ?ᶢ)
    (?ɥ . ?ᶣ) (?ɨ . ?ᶤ) (?ɩ . ?ᶥ) (?ɪ . ?ᶦ)
    (?ᵻ . ?ᶧ) (?ʝ . ?ᶨ) (?ɭ . ?ᶩ) (?ᶅ . ?ᶪ)
    (?ʟ . ?ᶫ) (?ɱ . ?ᶬ) (?ɰ . ?ᶭ) (?ɲ . ?ᶮ)
    (?ɳ . ?ᶯ) (?ɴ . ?ᶰ) (?ɵ . ?ᶱ) (?ɸ . ?ᶲ)
    (?ʂ . ?ᶳ) (?ʃ . ?ᶴ) (?ƫ . ?ᶵ) (?ʉ . ?ᶶ)
    (?ʊ . ?ᶷ) (?ᴜ . ?ᶸ) (?ʋ . ?ᶹ) (?ʌ . ?ᶺ)
    (?z . ?ᶻ) (?ʐ . ?ᶼ) (?ʑ . ?ᶽ) (?ʒ . ?ᶾ)
    (?θ . ?ᶿ) (?0 . ?⁰) (?i . ?ⁱ) (?4 . ?⁴)
    (?5 . ?⁵) (?6 . ?⁶) (?7 . ?⁷) (?8 . ?⁸)
    (?9 . ?⁹) (?+ . ?⁺) (?− . ?⁻) (?= . ?⁼)
    (?( . ?⁽) (?) . ?⁾) (?n . ?ⁿ) (?ⵡ . ?ⵯ)))
 ;; ("SM" . ?℠) ("TM" . ?™)

(defsubst lookup-superscript-character (char)
  "Return the superscript character of CHAR if exists."
  (cdr (assq char lookup-superscript-char-table)))

(defun lookup-superscript-string (str)
  (let ((i (string-to-list str)) chars ch)
    (while i
      (if (setq ch (lookup-superscript-character (car i)))
          (setq chars (cons ch chars) i (cdr i))
        (setq i nil chars nil)))
    (if chars (apply 'string (nreverse chars))
      (put-text-property
       0 (length str)
       'display '((raise 0.3) (height 0.8))
       str)
      str)))

(defconst lookup-subscript-char-table
  '((?i . ?ᵢ) (?r . ?ᵣ) (?u . ?ᵤ) (?v . ?ᵥ)
    (?β . ?ᵦ) (?γ . ?ᵧ) (?ρ . ?ᵨ) (?φ . ?ᵩ)
    (?χ . ?ᵪ) (?0 . ?₀) (?1 . ?₁) (?2 . ?₂)
    (?3 . ?₃) (?4 . ?₄) (?5 . ?₅) (?6 . ?₆)
    (?7 . ?₇) (?8 . ?₈) (?9 . ?₉) (?+ . ?₊)
    (?− . ?₋) (?= . ?₌) (?( . ?₍) (?) . ?₎)
    (?a . ?ₐ) (?e . ?ₑ) (?o . ?ₒ) (?x . ?ₓ)
    (?ə . ?ₔ)))

(defsubst lookup-subscript-character (char)
  "Return the subscript character of CHAR if exists."
  (cdr (assq char lookup-subscript-char-table)))

(defun lookup-subscript-string (str)
  (let ((i (string-to-list str)) chars ch)
    (while i
      (if (setq ch (lookup-subscript-character (car i)))
          (setq chars (cons ch chars) i (cdr i))
        (setq i nil chars nil)))
    (if chars (apply 'string (nreverse chars))
      (put-text-property
       0 (length str)
       'display '((raise -0.3) (height 0.8))
       str)
      str)))

(provide 'lookup-utils)

;;; lookup-utils.el ends here
