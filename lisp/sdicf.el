;;; sdicf.el --- Search library for SDIC format dictionary
;;; $Id: sdicf.el,v 1.2 2000/11/19 23:59:51 knishida Exp $

;; Copyright (C) 1999 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;	   NISHIDA Keisuke <knishida@ring.aist.go.jp>
;; Created: 1 Feb 1999
;; Version: 0.9
;; Keywords: dictionary

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; これは、SDIC形式辞書を検索するためのライブラリです。次の関数から成
;; ります。

;;     sdicf-open           - SDIC 辞書のオープン
;;     sdicf-close          - SDIC 辞書のクローズ
;;     sdicf-search         - SDIC 辞書から検索
;;     sdicf-entry-headword - エントリの見出し語を得る
;;     sdicf-entry-keywords - エントリの検索キーのリストを得る
;;     sdicf-entry-text     - エントリの本文を得る

;; それぞれの関数の詳細は、関数の説明文字列に記述されています。


;;; Note:
 
;; * GNU Emacs 19.30 以降であれば、`auto-compression-mode' を有効にする
;;   ことで、`direct' 方式で圧縮した辞書を用いることが出来る。展開は自
;;   動で行なわれるため、特別な設定は必要ありません。
;; 
;; * 速度重視のため `save-match-data' による一致データの退避と回復は一
;;   切していません。



;;;------------------------------------------------------------
;;;		Customizable variables
;;;------------------------------------------------------------

(defun sdicf-find-program (&rest programs)
  (if programs
      (catch 'which
	(mapcar (lambda (file)
		  (mapcar (lambda (path)
			    (if (file-executable-p (expand-file-name file path))
				(throw 'which (expand-file-name file path))))
			  exec-path))
		programs))))

(defvar sdicf-egrep-command (sdicf-find-program "egrep" "egrep.exe" "grep" "grep.exe")
  "*Executable file name of egrep")

(defvar sdicf-fgrep-command (sdicf-find-program "fgrep" "fgrep.exe" "grep" "grep.exe")
  "*Executable file name of fgrep")

(defvar sdicf-array-command (sdicf-find-program "array" "array.exe")
  "*Executable file name of array")

(defvar sdicf-sass-command (sdicf-find-program "sass" "sass.exe")
  "*Executable file name of sass")

(defvar sdicf-default-coding-system
  (if (>= emacs-major-version 20)
      (if (featurep 'mule)
	  (if (string-match "XEmacs" emacs-version)
	      (cond
	       ((memq 'euc-japan-unix (coding-system-list)) 'euc-japan-unix)
	       ((memq 'euc-jp-unix (coding-system-list)) 'euc-jp-unix))
	    'euc-japan-unix))
    (and (boundp 'MULE) *euc-japan*unix))
  "*Default coding system for sdicf.el")

;; Error Symbols
(put 'sdicf-missing-file 'error-conditions '(error sdicf-errors sdicf-missing-file))
(put 'sdicf-missing-file 'error-message "Can't find file")
(put 'sdicf-missing-executable 'error-conditions '(error sdicf-errors sdicf-missing-executable))
(put 'sdicf-missing-executable 'error-message "Can't find executable")
(put 'sdicf-invalid-strategy 'error-conditions '(error sdicf-errors sdicf-invalid-strategy))
(put 'sdicf-invalid-strategy 'error-message "Invalid search strategy")
(put 'sdicf-decide-strategy 'error-conditions '(error sdicf-errors sdicf-decide-strategy))
(put 'sdicf-decide-strategy 'error-message "Can't decide strategy automatically")
(put 'sdicf-invalid-method 'error-conditions '(error sdicf-errors sdicf-invalid-method))
(put 'sdicf-invalid-method 'error-message "Invalid search method")



;;;------------------------------------------------------------
;;;		Internal variables
;;;------------------------------------------------------------

(defconst sdicf-version "0.9" "Version number of sdicf.el")

(defconst sdicf-strategy-alist
  '((sass sdicf-sass-available sdicf-sass-init sdicf-sass-quit sdicf-sass-search)
	(array sdicf-array-available sdicf-array-init sdicf-array-quit sdicf-array-search)
    (grep sdicf-grep-available sdicf-grep-init sdicf-grep-quit sdicf-grep-search)
    (direct sdicf-direct-available sdicf-direct-init sdicf-direct-quit sdicf-direct-search))
  "利用できる strategy の連想配列
配列の各要素は、
    strategy のシンボル
    strategy の利用可能性を検査する関数
    strategy を初期化する関数
    strategy を終了する関数
    strategy を使って検索する関数
の4つの要素からなるリストとなっている。strategy の自動判定を行うときは、
この連想配列に先に登録されている strategy が使われる。")



;;;------------------------------------------------------------
;;;		Internal functions
;;;------------------------------------------------------------

(if (fboundp 'buffer-live-p)
    (defalias 'sdicf-buffer-live-p 'buffer-live-p)
  (defun sdicf-buffer-live-p (object) "\
Return non-nil if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed."
    (and object (bufferp object) (buffer-name object))))

(defsubst sdicf-object-p (sdic)
  "辞書オブジェクトかどうか検査する"
  (and (vectorp sdic) (eq 'SDIC (aref sdic 0))))

(defsubst sdicf-entry-p (entry)
  (and (stringp entry) (string-match "^<.>\\([^<]+\\)</.>" entry)))

(defsubst sdicf-get-filename (sdic)
  "辞書オブジェクトからファイル名を得る"
  (aref sdic 1))

(defsubst sdicf-get-coding-system (sdic)
  "辞書オブジェクトから coding-system を得る"
  (aref sdic 2))

(defsubst sdicf-get-strategy (sdic)
  "辞書オブジェクトから strategy を得る"
  (aref sdic 3))

(defsubst sdicf-get-buffer (sdic)
  "辞書オブジェクトから検索用バッファを得る"
  (aref sdic 4))

(defun sdicf-common-init (sdic) "\
共通の辞書初期化関数
作業用バッファが存在することを確認し、なければ新しく生成する。作業用バッ
ファを返す。"
  (or (and (sdicf-buffer-live-p (sdicf-get-buffer sdic))
	   (sdicf-get-buffer sdic))
      (let ((buf (generate-new-buffer (format " *sdic %s*" (sdicf-get-filename sdic)))))
	(buffer-disable-undo buf)
	(aset sdic 4 buf))))

(defun sdicf-common-quit (sdic) "\
共通の辞書終了関数"
  (if (sdicf-buffer-live-p (sdicf-get-buffer sdic)) (kill-buffer (sdicf-get-buffer sdic))))

(defsubst sdicf-search-internal () "\
現在行をチェックし、エントリならば現在行の内容を entries に加える。
ポイントを行の先頭に移動しておかなければならない。関数の実行後、ポイン
トは次の行頭に移動する。"
  (if (eq (following-char) ?<)
      (progn
	(setq entries (cons (buffer-substring (point) (progn (end-of-line) (point))) entries))
	(forward-char))
    (forward-line)))

(defun sdicf-encode-string (string) "\
STRING をエンコードする
エンコードした文字列を返す"
  (let ((start 0) ch list)
    (while (string-match "[&<>\n]" string start)
      (setq ch (aref string (match-beginning 0))
	    list (cons (if (eq ch ?&) "&amp;"
			 (if (eq ch ?<) "&lt;"
			   (if (eq ch ?>) "&gt;" "&lf;")))
		       (cons (substring string start (match-beginning 0)) list))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) list))))))

(defun sdicf-decode-string (string) "\
STRING をデコードする
デコードした文字列を返す"
  (let ((start 0) list)
    (while (string-match "&\\(\\(lt\\)\\|\\(gt\\)\\|\\(lf\\)\\|\\(amp\\)\\);" string start)
      (setq list (cons (if (match-beginning 2) "<"
			 (if (match-beginning 3) ">"
			   (if (match-beginning 4) "\n" "&")))
		       (cons (substring string start (match-beginning 0)) list))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) list))))))

(defun sdicf-insert-file-contents (filename coding-system &optional visit beg end replace) "\
CODING-SYSTEM を明示的に指定して insert-file-contents を呼び出す
CODING-SYSTEM 以外の引数の意味は insert-file-contents と同じ"
  (let ((coding-system-for-read coding-system)
	(file-coding-system-for-read coding-system))
    (insert-file-contents filename visit beg end replace)))

(defun sdicf-call-process (program coding-system &optional infile buffer display &rest args) "\
CODING-SYSTEM を明示的に指定して call-process を呼び出す
CODING-SYSTEM 以外の引数の意味は call-process と同じ"
  (let ((coding-system-for-read coding-system)
	(coding-system-for-write coding-system)
	(process-input-coding-system coding-system)
	(process-output-coding-system coding-system)
	(file-name-coding-system coding-system)
	(default-process-coding-system (cons coding-system coding-system)))
    (apply 'call-process program infile buffer display args)))

(defun sdicf-start-process (name buffer program coding-system &rest args) "\
start-process を実行した後、生成されたプロセスに CODING-SYSTEM を設定する
CODING-SYSTEM 以外の引数の意味は start-process と同じ"  
  (let ((proc (apply 'start-process name buffer program args)))
    (if (fboundp 'set-process-coding-system)
	(set-process-coding-system proc coding-system coding-system)
      (set-process-input-coding-system proc coding-system)
      (set-process-output-coding-system proc coding-system))
    proc))



;;; Strategy `direct'

(defun sdicf-direct-available (sdic)
  (or (file-readable-p (sdicf-get-filename sdic))
      (signal 'sdicf-missing-file (list (sdicf-get-filename sdic)))))

(defun sdicf-direct-init (sdic)
  (or (sdicf-buffer-live-p (sdicf-get-buffer sdic))
      (save-excursion
	(sdicf-common-init sdic)
	(set-buffer (sdicf-get-buffer sdic))
	(delete-region (point-min) (point-max))
	(sdicf-insert-file-contents (sdicf-get-filename sdic) (sdicf-get-coding-system sdic))
	(while (re-search-forward "^#" nil t)
	  (delete-region (1- (point)) (progn (end-of-line) (min (1+ (point)) (point-max)))))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	t)))

(defalias 'sdicf-direct-quit 'sdicf-common-quit)

(defun sdicf-direct-search (sdic pattern &optional case regexp) "\
検索対象のファイルをバッファに読み込んで検索を行う

見つかったエントリのリストを返す。CASE が nil ならば、大文字小文字の違
いを区別して検索する。REGEXP が Non-nil ならば、PATTERN を正規表現と見
なして検索する。"
  (sdicf-direct-init sdic)
  (save-excursion
    (set-buffer (sdicf-get-buffer sdic))
    (let ((case-fold-search case) entries)
      (goto-char (point-min))
      (if regexp
	  (while (re-search-forward pattern nil t)
	    (forward-line 0)
	    (sdicf-search-internal))
	(while (search-forward pattern nil t)
	  (forward-line 0)
	  (sdicf-search-internal)))
      (nreverse entries))))



;;; Strategy `grep'

(defun sdicf-grep-available (sdic)
  (and (or (file-readable-p (sdicf-get-filename sdic))
	   (signal 'sdicf-missing-file (list (sdicf-get-filename sdic))))
       (or (and (stringp sdicf-fgrep-command)
		(file-executable-p sdicf-fgrep-command))
	   (signal 'sdicf-missing-executable '(fgrep grep)))
       (or (and (stringp sdicf-egrep-command)
		(file-executable-p sdicf-egrep-command))
	   (signal 'sdicf-missing-executable '(egrep grep)))))

(defalias 'sdicf-grep-init 'sdicf-common-init)

(defalias 'sdicf-grep-quit 'sdicf-common-quit)

(defun sdicf-grep-search (sdic pattern &optional case regexp) "\
fgrep / egrep または grep を使って検索を行う

見つかったエントリのリストを返す。CASE が nil ならば、大文字小文字の違
いを区別して検索する。REGEXP が nil ならば sdicf-fgrep-command で指定
されたコマンドを使って検索する。REGEXP が Non-nil ならば 
sdicf-egrep-command で指定されたコマンドを使う。"
  (sdicf-grep-init sdic)
  (save-excursion
    (set-buffer (sdicf-get-buffer sdic))
    (delete-region (point-min) (point-max))
    (apply 'sdicf-call-process
	   (if regexp sdicf-egrep-command sdicf-fgrep-command)
	   (sdicf-get-coding-system sdic)
	   nil t nil
	   (if regexp (if case (list "-i" "-e" pattern (sdicf-get-filename sdic))
			(list "-e" pattern (sdicf-get-filename sdic)))
	     (if case (list "-i" pattern (sdicf-get-filename sdic))
	       (list "-e" pattern (sdicf-get-filename sdic)))))
    (goto-char (point-min))
    (let (entries)
      (while (not (eobp)) (sdicf-search-internal))
      (nreverse entries))))


;;; Strategy `sass'

(defun sdicf-sass-available (sdic)
  (and (or (file-readable-p (sdicf-get-filename sdic))
	   (signal 'sdicf-missing-file (list (sdicf-get-filename sdic))))
       (or (file-readable-p (concat (sdicf-get-filename sdic) ".ary"))
	   (signal 'sdicf-missing-file (list (concat (sdicf-get-filename sdic) ".ary"))))
       (or (and (stringp sdicf-sass-command)
		(file-executable-p sdicf-sass-command))
	   (signal 'sdicf-missing-executable '(sass)))))

(defalias 'sdicf-sass-init 'sdicf-common-init)

(defalias 'sdicf-sass-quit 'sdicf-common-quit)

(defun sdicf-sass-search (sdic pattern &optional case regexp) "\
sass を使って検索を行う

見つかったエントリのリストを返す。CASE が nil ならば、大文字小文字の違
いを区別して検索する。REGEXP が nil ならば sdicf-fgrep-command で指定
されたコマンドを使って検索する。REGEXP が Non-nil ならば 
sdicf-egrep-command で指定されたコマンドを使う。"
  (sdicf-sass-init sdic)
  (save-excursion
    (set-buffer (sdicf-get-buffer sdic))
    (delete-region (point-min) (point-max))
    (apply 'sdicf-call-process
	   sdicf-sass-command
	   (sdicf-get-coding-system sdic)
	   nil t nil
	   (if regexp (if case (list "-i" "-r" pattern (sdicf-get-filename sdic))
			(list "-r" pattern (sdicf-get-filename sdic)))
	     (if case (list "-i" pattern (sdicf-get-filename sdic))
	       (list "-r" pattern (sdicf-get-filename sdic)))))
    (goto-char (point-min))
    (let (entries)
      (while (not (eobp)) (sdicf-search-internal))
      (nreverse entries))))



;;; Strategy `array'

(defun sdicf-array-available (sdic)
  (and (or (file-readable-p (sdicf-get-filename sdic))
	   (signal 'sdicf-missing-file (list (sdicf-get-filename sdic))))
       (or (file-readable-p (concat (sdicf-get-filename sdic) ".ary"))
	   (signal 'sdicf-missing-file (list (concat (sdicf-get-filename sdic) ".ary"))))
       (or (and (stringp sdicf-array-command)
		(file-executable-p sdicf-array-command))
	   (signal 'sdicf-missing-executable '(array)))))

(defun sdicf-array-init (sdic)
  (sdicf-common-init sdic)
  (let ((proc (get-buffer-process (sdicf-get-buffer sdic))))
    (or (and proc (eq (process-status proc) 'run))
	(progn
	  (setq proc (sdicf-start-process "array"
					  (sdicf-get-buffer sdic)
					  sdicf-array-command
					  (sdicf-get-coding-system sdic)
					  (sdicf-get-filename sdic)))
	  (accept-process-output proc)
	  (process-send-string proc "style line\n")
	  (accept-process-output proc)
	  (process-send-string proc "order index\n")
	  (accept-process-output proc)
	  (process-kill-without-query proc)
	  (set-process-filter proc 'sdicf-array-wait-prompt)
	  t))))

(defun sdicf-array-quit (sdic)
  (if (sdicf-buffer-live-p (sdicf-get-buffer sdic))
      (let ((proc (get-buffer-process (sdicf-get-buffer sdic))))
	(and proc
	     (eq (process-status proc) 'run)
	     (set-process-filter proc nil)
	     (process-send-string proc "quit\n"))
	(kill-buffer (sdicf-get-buffer sdic)))))

(defun sdicf-array-send-string (proc string) "\
指定された文字列 STRING をコマンドとして PROC に渡してプロンプトが現れるまで待つ関数"
  (save-excursion
    (let ((sdicf-array-wait-prompt-flag t))
      (set-buffer (process-buffer proc))
      (set-marker (process-mark proc) (point-max))
      (process-send-string proc (concat string "\n"))
      (while sdicf-array-wait-prompt-flag (accept-process-output proc)))))

(defun sdicf-array-wait-prompt (proc string) "\
プロンプト ok が現れたことを検知して、sdicf-array-wait-prompt-flag を nil にするフィルタ関数"
  (save-excursion
    (save-match-data ; Emacs-19.34 以降は自動的に検索結果の待避/回復が行われるので不要
      (set-buffer (process-buffer proc))
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))
      (skip-chars-backward " \t\n")
      (forward-line 0)
      (if (looking-at "ok\n")
	  (setq sdicf-array-wait-prompt-flag nil))
      )))

(defun sdicf-array-search (sdic pattern &optional case regexp) "\
array を使って検索を行う

見つかったエントリのリストを返す。array は正規表現検索および大文字小文
字の違いを区別しない検索は出来ない。従って、CASE が Non-nil の場合は、
大文字小文字を区別して検索した場合の結果を返す。REGEXP が Non-nil の場
合は空りストを返す。"
  (sdicf-array-init sdic)
  (if regexp
      (signal 'sdicf-invalid-method '(regexp))
    (save-excursion
      (let ((proc (get-buffer-process (set-buffer (sdicf-get-buffer sdic))))
	    (case-fold-search nil))
	(sdicf-array-send-string proc "init")
	(delete-region (point-min) (point-max))
	(sdicf-array-send-string proc (concat "search " pattern))
	(if (looking-at "FOUND:")
	    (progn
	      (delete-region (point-min) (point-max))
	      (sdicf-array-send-string proc "show")
	      (let (entries cons)
		(while (not (eobp)) (sdicf-search-internal))
		(setq entries (sort entries 'string<)
		      cons entries)
		(while (cdr cons)
		  (if (equal (car cons) (car (cdr cons)))
		      (setcdr cons (cdr (cdr cons)))
		    (setq cons (cdr cons))))
		entries)))))))


;;;------------------------------------------------------------
;;;		Interface functions
;;;------------------------------------------------------------

(defun sdicf-open (filename &optional coding-system strategy) "\
SDIC形式の辞書をオープンする

FILENAME は辞書のファイル名。STRATEGY は検索を行なう方式を指定する引数
で、次のいずれかの値を取る。

    `direct' - 辞書をバッファに読んで直接検索。
    `grep'   - grep コマンドを用いて検索。
    `array'  - SUFARY を用いた高速検索。

STRATEGY が省略された場合は sdicf-strategy-alist の値を使って自動的に
判定する。CODING-SYSTEM が省略された場合は、sdicf-default-coding-system
の値を使う。

SDIC 辞書オブジェクトは CAR が `SDIC' のベクタである。以下の4つの要素
を持つ。
    ・ファイル名
    ・辞書の coding-system
    ・strategy
    ・作業用バッファ
"
  (let ((sdic (vector 'SDIC filename (or coding-system sdicf-default-coding-system) nil nil)))
    (aset sdic 3 (if strategy
		     (if (assq strategy sdicf-strategy-alist)
			 (if (funcall (nth 1 (assq strategy sdicf-strategy-alist)) sdic)
			     strategy)
		       (signal 'sdicf-invalid-strategy (list strategy)))
		   (catch 'found-strategy
		     (mapcar (lambda (e)
			       (if (condition-case nil
				       (funcall (nth 1 e) sdic)
				     (sdicf-errors nil))
				   (throw 'found-strategy (car e))))
			     sdicf-strategy-alist)
		     (signal 'sdicf-decide-strategy nil))))
    sdic))

(defun sdicf-close (sdic)
  "SDIC形式の辞書をクローズする"
  (or (sdicf-object-p sdic)
      (signal 'wrong-type-argument (list 'sdicf-object-p sdic)))
  (funcall (nth 3 (assq (sdicf-get-strategy sdic) sdicf-strategy-alist)) sdic))

(defun sdicf-search (sdic method word) "\
SDIC形式の辞書から WORD をキーとして検索を行う

見付かったエントリのリストを返す。METHOD は検索法で、次のいずれかの値
を取る。

    `prefix' - 前方一致検索
    `suffix' - 後方一致検索
    `exact'  - 完全一致検索
    `text'   - 全文検索
    `regexp' - 正規表現検索

前方一致検索、後方一致検索、完全一致検索の場合は大文字/小文字を区別し
て検索を行う。全文検索および正規表現検索の場合は、case-fold-search の
値によって変化する。ただし、strategy によっては、指定された検索方式に
対応していない場合があるので、注意すること。対応していない場合の返り値
は、strategy による。"
  (or (sdicf-object-p sdic)
      (signal 'wrong-type-argument (list 'sdicf-object-p sdic)))
  (or (stringp word)
      (signal 'wrong-type-argument (list 'stringp word)))
  (let ((case-fold-search (if (eq method 'text) case-fold-search)))
    (funcall (nth 4 (assq (sdicf-get-strategy sdic) sdicf-strategy-alist))
	     sdic
	     (cond
	      ((eq method 'prefix) (concat "<K>" (sdicf-encode-string (downcase word))))
	      ((eq method 'suffix) (concat (sdicf-encode-string (downcase word)) "</K>"))
	      ((eq method 'exact) (concat "<K>" (sdicf-encode-string (downcase word)) "</K>"))
	      ((eq method 'text) word)
	      ((eq method 'regexp) word)
	      (t (signal 'sdicf-invalid-method (list method))))
	     (and (or (eq method 'text) (eq method 'regexp)) case-fold-search)
	     (eq method 'regexp))))

(defun sdicf-entry-headword (entry)
  "エントリ ENTRY の見出し語を返す。"
  (or (sdicf-entry-p entry)
      (signal 'wrong-type-argument (list 'sdicf-entry-p entry)))
  (sdicf-decode-string (substring entry (match-beginning 1) (match-end 1))))

(defun sdicf-entry-keywords (entry &optional add-headword) "\
エントリ ENTRY の検索キーのリストを返す
ADD-HEADWORD が Non-nil の場合は検索キーに見出し語を加えたリストを返す"
  (or (sdicf-entry-p entry)
      (signal 'wrong-type-argument (list 'sdicf-entry-p entry)))
  (let ((start (match-end 0))
	(keywords (if (or add-headword (string= "<K>" (substring entry 0 3)))
		      (list (sdicf-decode-string (substring entry (match-beginning 1) (match-end 1)))))))
    (while (eq start (string-match "<.>\\([^<]+\\)</.>" entry start))
      (setq start (match-end 0)
	    keywords (cons (sdicf-decode-string (substring entry (match-beginning 1) (match-end 1))) keywords)))
    (nreverse keywords)))

(defun sdicf-entry-text (entry)
  "エントリ ENTRY の本文を返す。"
  (or (stringp entry)
      (signal 'wrong-type-argument (list 'stringp entry)))
  (sdicf-decode-string (substring entry (string-match "[^>]*$" entry))))


(provide 'sdicf)

;;; sdicf.el ends here
