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

;; alist by assq

(defsubst lookup-assq-get (alist key)
  "Return the value associated with KEY in ALIST.
This compares keys with `eq'.  See `assq'."
  (cdr (assq key alist)))

(defsubst lookup-assq-del (alist key)
  "Delete any entry in ALIST associated with KEY.
This compares keys with `eq'.  See `assq'."
  (delq (assq key alist) alist))

(defun lookup-assq-put (alist key value)
  "Associate KEY in ALIST with VALUE.
This compares keys with `eq'.  See `assq'."
  (if value
      (cons (cons key value) (lookup-assq-del alist key))
    (lookup-assq-del alist key)))

;; alist by assoc

(defsubst lookup-assoc-get (alist key)
  "Return the value associated with KEY in ALIST.
This compares keys with `equal'.  See `assoc'."
  (cdr (assoc key alist)))

(defsubst lookup-assoc-del (alist key)
  "Delete any entry in ALIST associated with KEY.
This compares keys with `equal'.  See `assoc'."
  (delq (assoc key alist) alist))

(defun lookup-assoc-put (alist key value)
  "Associate KEY in ALIST with VALUE.
This compares keys with `equal'.  See `assoc'."
  (if value
      (cons (cons key value) (lookup-assoc-del alist key))
    (lookup-assoc-del alist key)))

;; alist set/ref

(defsubst lookup-assq-ref (symbol key)
  "Like `lookup-assq-get', except SYMBOL should be a symbol.
SYMBOL's value must be an alist."
  (lookup-assq-get (symbol-value symbol) key))

(defsubst lookup-assq-set (symbol key value)
  "Like `lookup-assq-put', except SYMBOL should be a symbol.
SYMBOL's value will be modified by this function."
  (set symbol (lookup-assq-put (symbol-value symbol) key value)))

(defsubst lookup-assoc-ref (symbol key)
  "Like `lookup-assoc-get', except SYMBOL should be a symbol.
SYMBOL's value must be an alist."
  (lookup-assoc-get (symbol-value symbol) key))

(defsubst lookup-assoc-set (symbol key value)
  "Like `lookup-assoc-put', except SYMBOL should be a symbol.
SYMBOL's value will be modified by this function."
  (set symbol (lookup-assoc-put (symbol-value symbol) key value)))

;; multi put/get

(defsubst lookup-multi-put (symbol &rest args)
  "`put' に似ているが、任意の数の属性キーを取ることが出来る。
例えば (lookup-multi-put SYMBOL KEY1 KEY2 VALUE) という式は、
SYMBOL における KEY1 と KEY2 というキーに対する値として VALUE を設定する。
設定された値は `lookup-multi-get' により参照出来る。"
  (set symbol (lookup-multi-put-1 (symbol-value symbol) args)))

(defun lookup-multi-put-1 (alist args)
  ;; `lookup-multi-put' の内部関数。
  (if (cddr args)
      (lookup-assq-put alist (car args)
		       (lookup-multi-put-1 (lookup-assq-get alist (car args))
					   (cdr args)))
    (lookup-assq-put alist (car args) (cadr args))))

(defsubst lookup-multi-get (symbol &rest args)
  "`get' に似ているが、任意の数の属性キーを取ることが出来る。
例えば (lookup-multi-put SYMBOL KEY1 KEY2) という式は、
SYMBOL における KEY1 と KEY2 というキーに対する値を参照する。
`lookup-multi-put' を参照。"
  (lookup-multi-get-1 (symbol-value symbol) args))

(defun lookup-multi-get-1 (alist args)
  ;; `lookup-multi-get' の内部関数。
  (if args
      (lookup-multi-get-1 (lookup-assq-get alist (car args)) (cdr args))
    alist))

;; misc

(defun lookup-repq (list from to)
  "LIST の中で FROM に一致するオブジェクトを TO で置き換えたリストを生成する。
オブジェクトの比較は `eq' によって行なわれる。"
  (let ((result nil))
    (while list
      (setq result (cons (if (eq from (car list)) to (car list)) result)
	    list (cdr list)))
    (nreverse result)))

(defun lookup-nunique (list &optional predicate)
  "LIST の中で重複するオブジェクトを取り除き一つにする。
オブジェクトの比較は PREDICATE によって行なわれる。省略された場合には
`equal' が用いられる。LIST は上書き変更される。

 (lookup-nunique '(a a b c b c a))  => (a b c)"
  (let ((top list) first rest)
    (setq predicate (or predicate 'equal))
    (while list
      (setq first (car list) rest list)
      (while (cdr rest)
	(if (funcall predicate first (cadr rest))
	    (setcdr rest (cddr rest))
	  (setq rest (cdr rest))))
      (setq list (cdr list)))
    top))

(defun lookup-foreach (function list)
  "LIST の全ての要素に対して (funcall FUNCTION ELEMENT) を実行する。
`mapcar' と似ているが何も値を返さず、単なる繰り返しに用いる。"
  (while list
    (funcall function (car list))
    (setq list (cdr list))))

(defun lookup-grep (predicate list)
  "LIST の全ての要素に対して (funcall PREDICATE ELEMENT) を実行し、
non-nil を返したものだけを新たにリストとして生成する。

 (lookup-grep 'symbolp '(1 a \"b\" c))  => (a c)"
  (let ((value nil))
    (while list
      (if (funcall predicate (car list))
	  (setq value (cons (car list) value)))
      (setq list (cdr list)))
    (nreverse value)))

(defun lookup-map-until (predicate list)
  "LIST の全ての要素に対して (funcall PREDICATE ELEMENT) を実行し、
最初に non-nil を返した要素を返す。"
  (let ((value nil))
    (while list
      (if (funcall predicate (car list))
	  (setq value (car list) list nil)
	(setq list (cdr list))))
    value))

(defun lookup-reverse-string (string)
  "STRING の前後の順を入れ替えた文字列を生成する。"
  (concat (nreverse (string-to-list string))))

(defun lookup-oneline-string (string)
  "STRING 内の改行をスペース一文字で置き換え、一行にする。"
  (while (string-match "\n *" string)
    (setq string (replace-match " " t t string)))
  string)

(defun lookup-read-string (prompt &optional init history default inherit)
  "`read-string' に似ているが、オプション DEFAULT が指定されば場合、
プロンプトにその値を (defaut DEFAULT) のように表示する。PROMPT には
自動的に \": \" が付加される。"
  (read-string (if default
		   (concat prompt " (default " default "): ")
		 (concat prompt ": "))
	       init history default inherit))

(defun lookup-map-over-property (from to prop func &optional object)
  "FROM から TO までのリージョンにある各 PROP に対して、FUNC を実行する。
FUNC は (funcall FUNC START END VALUE) のように呼び出される。START は
PROP の開始地点。END は終了地点。VALUE は property の値。
オプションの OBJECT が指定されて場合、current-buffer ではなく OBJECT の
中から PROP を探し出す。"
  (let ((beg from) end value)
    (while (setq value (get-text-property beg prop object)
		 end (text-property-not-all beg to prop value object))
      (if value (funcall func beg end value))
      (setq beg end))
    (if value (funcall func beg to value))))

(put 'lookup-with-coding-system 'lisp-indent-function 1)
(defmacro lookup-with-coding-system (coding &rest body)
  "入出力の文字コードを CODING に設定して BODY を実行する。"
  (` (let (;; for GNU Emacs 20 and XEmacs 20
	   (coding-system-for-read (, coding))
	   (coding-system-for-write (, coding))
	   ;; for Mule 2.3
	   (process-input-coding-system (, coding))
	   (process-output-coding-system (, coding))
	   (default-process-coding-system (cons (, coding) (, coding))))
       (,@ body))))

(put 'lookup-with-buffer-and-window 'lisp-indent-function 1)
(defmacro lookup-with-buffer-and-window (buffer &rest body)
  "current-buffer を BUFFER に設定し、更に selected-window を BUFFER の
window に設定した状態で BODY を実行する。"
  (` (with-current-buffer (, buffer)
       (save-selected-window
	 (if (get-buffer-window (, buffer))
	     (select-window (get-buffer-window (, buffer)))
	   (error "No window for buffer `%s'" (, buffer)))
	 (,@ body)))))

(defun lookup-parse-table (func start end)
  (let ((table nil) value)
    (goto-char start)
    (while (re-search-forward " *\\([^:\n]+\\): *\\(.*\\)" end t)
      (if (setq value (funcall func (match-string 1) (match-string 2)))
	  (setq table (cons value table))))
    (nreverse table)))

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
    (lookup-foreach (lambda (args)
		      (lookup-foreach (lambda (pair)
					(setq width (string-width
						     (nth (car pair) args)))
					(if (< (cddr pair) width)
					    (setcdr (cdr pair) width)))
				      width-alist))
		    args-list)
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
    (while args-list
      (insert (apply 'format format (car args-list)))
      (setq args-list (cdr args-list)))))

;;;
;;; Lookup current-word
;;;

(defun lookup-current-word (&optional strict)
  "バッファのカーソル位置かその周辺にある単語を文字列として返す。
オプションの引数 STRICT に non-nil が指定された場合、カーソルが単語と
重なっている場合のみを対象とする。
変数 `lookup-use-kakasi' が non-nil に指定された場合、日本語の単語も
それなりに判定して切り出す。"
  (save-excursion
    (unless (or strict (eq (char-syntax (or (char-after (point)) 0))
			   (if (boundp 'MULE) ?e ?w)))
      (let ((syntax (if (boundp 'MULE) "^e" "^w")))
	(skip-syntax-backward syntax (save-excursion (beginning-of-line) (point)))
	(if (bolp)
	    (skip-syntax-forward syntax (save-excursion (end-of-line) (point)))
	  (backward-char))))
    (let* ((ch (or (char-after (point)) 0))
	   ;; APEL が定義している char-charset は Emacs 20 のそれと
	   ;; 返り値が異なるので回避する。
	   (charset (if (not (fboundp 'char-leading-char))
			(char-charset ch)
		      (setq ch (char-leading-char ch))
		      (cond ((eq ch 0) 'ascii)
			    ((eq ch 146) 'japanese-jisx0208)
			    (t ch)))))
      (cond ((eq charset 'ascii) (lookup-current-word-ascii))
	    ((eq charset 'japanese-jisx0208) (lookup-current-word-japanese))
	    (t (lookup-current-word-general))))))

(defun lookup-current-word-general ()
  ;; `lookup-current-word' の内部関数。
  ;; syntax が "w" である文字の連なりを単語として切り出す。
  (if (fboundp 'thing-at-point)
      (thing-at-point 'word)
    (buffer-substring-no-properties
     (progn (skip-syntax-backward "w") (point))
     (progn (skip-syntax-forward "w") (point)))))

(defun lookup-current-word-ascii ()
  ;; `lookup-current-word' の内部関数。
  ;; アルファベットあるいは数字の連なりを単語として切り出す。
  ;; もし単語の末尾が `-' であり、更にそれが行末であるならば(つまり、
  ;; スペルが区切られているようなら)、次の行の最初のワードと連結する。
  (let ((word (buffer-substring-no-properties
	       (progn (skip-chars-backward "a-zA-Z0-9") (point))
	       (progn (skip-chars-forward "a-zA-Z0-9") (point)))))
    (if (not (looking-at "-\n"))
	word
      (forward-line)
      (concat word (buffer-substring-no-properties
		    (progn (skip-chars-forward "^a-zA-Z0-9\n") (point))
		    (progn (skip-chars-forward "a-zA-Z0-9") (point)))))))

(defun lookup-current-word-japanese ()
  ;; `lookup-current-word' の内部関数。
  ;; `lookup-use-kakasi' が non-nil に設定されている場合、KAKASI を用いて
  ;; 日本語の単語をそれなりに切り出す。そうでない場合は、
  ;; `lookup-current-word-general' を呼び出す。
  (if (not lookup-use-kakasi)
      (lookup-current-word-general)
    (let ((kakasi (if (stringp lookup-use-kakasi) lookup-use-kakasi "kakasi"))
	  (temp-buffer (lookup-temp-buffer))
	  (syntax (if (boundp 'MULE) "e" "w"))
	  (start (point)) (n 1) regexp)
      (lookup-with-coding-system lookup-kakasi-coding-system
	(call-process-region
	 (progn (skip-syntax-backward syntax) (point))
	 (progn (skip-syntax-forward syntax) (point))
	 kakasi nil temp-buffer nil "-w"))
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

;; Description:
;; 
;; Lookup のいくつかの agent が用いているプロセス操作関数。
;; これを使うことは必須ではないが、コマンドを発行して出力を受け取る
;; ようなよくあるタイプの入出力を簡略化するのに便利。

;; Functions:
;;
;; lookup-process-require - プロセスに文字列を送信し、出力を待つ。
;; lookup-process-kill - プロセスを終了する。

(defvar lookup-process-start-point nil)
(defvar lookup-process-output-value nil)
(defvar lookup-process-output-filter nil)
(defvar lookup-process-output-finished nil)
(defvar lookup-process-output-separator nil)
(defvar lookup-process-output-separator-lines 2)

(defun lookup-process-require (process string separator &optional filter)
  "PROCESS に対して STRING を送り、その出力を待つ。
出力終了の合図は、PROCESS が特定の終了コードを返したかどうかを判定する
ことによって行なう。SEPARATOR には終了コードとマッチするような正規表現を
指定する。これは三行以上にマッチするようなものであってはならない。もし
それ以上の大きさがあるなら、変数 `lookup-process-output-separator-lines'
の値を変更する。
オプション FILTER が設定された場合、出力終了と同時に FILTER を呼び出す。
FILTER は (funcall FILTER PROCESS) のように呼び出される。そのときの
current-buffer はプロセスの出力が書き込まれたバッファであり、出力部分に
対して narrowing されいる。SEPARATOR 自体は narrow 部分に含まれない。
FILTER の返却値が関数の値として返される。FILTER は process-filter として
本体とは別のスレッドで実行されるので注意。FILTER が省略された場合には、
プロセスの出力をそのまま文字列として返す。"
  (setq lookup-process-output-value nil)
  (setq lookup-process-output-filter filter)
  (setq lookup-process-output-finished nil)
  (setq lookup-process-output-separator separator)
  (let (temp-buffer)
    ;; プロセスにバッファが設定されている場合、そのバッファを出力の
    ;; 書き込みに用いる。そうでない場合、一時バッファを生成する。
    (unless (process-buffer process)
      (setq temp-buffer (lookup-temp-buffer))
      (set-process-buffer process temp-buffer))
    (with-current-buffer (process-buffer process)
      ;; FILTER 関数などで出力内容の処理中に、外字情報の獲得などのために
      ;; 多重にプロセスが呼ばれる場合がある。それを配慮して、実行時状態を
      ;; 保持しておく。[FIXME: XEmacs ではうまくいかない]
      (save-excursion
	(save-restriction
	  (save-match-data
	    (widen)
	    (goto-char (point-max))
	    ;; 送信文字列をバッファに保持してから、プロセスに送信する。
	    ;; これはデバッグ等のときに役立つ。
	    (insert string)
	    (setq lookup-process-start-point (point))
	    (set-process-filter process 'lookup-process-accept)
	    (process-send-string process string)
	    ;; 処理が終わるまで待機する。マルチスレッドに出来ると
	    ;; いいんだけどなぁ・・
	    (while (not lookup-process-output-finished)
	      (unless (accept-process-output process 5)
		(with-current-buffer (process-buffer process)
		  (when (> (point) lookup-process-start-point)
		    ;; 何らかの応答はあったのに、その後 5 秒以上何も起こら
		    ;; なかったとしたら、たぶん separator の指定がおかしくて
		    ;; 止まっている。
		    (error "Lookup BUG!! Report to the mailing list")))))
	    ;; 一時バッファを用いた場合、kill-buffer する。
	    (when temp-buffer
	      (set-process-buffer process nil)
	      (kill-buffer temp-buffer))))))
    lookup-process-output-value))

(defun lookup-process-accept (process string)
  ;; プロセスの出力を処理する。`lookup-process-require' が文字列を送信後、
  ;; プロセスが何か出力を返す度にこの関数が呼び出される。
  (with-current-buffer (process-buffer process)
    (insert string)
    ;; 出力に `lookup-process-output-separator' が含まれているかどうか、
    ;; 最後の数行だけチェックする。
    (forward-line (- lookup-process-output-separator-lines))
    (if (< (point) lookup-process-start-point)
	(goto-char lookup-process-start-point))
    (when (re-search-forward lookup-process-output-separator nil 0)
      (goto-char (match-beginning 0))
      (if lookup-process-output-filter
	  (save-current-buffer
	    (narrow-to-region lookup-process-start-point (point))
	    (goto-char (point-min))
	    (setq lookup-process-output-value
		  (funcall lookup-process-output-filter process))
	    (widen))
	(setq lookup-process-output-value
	      (buffer-substring lookup-process-start-point (point))))
      (setq lookup-process-output-finished t))))

(defun lookup-process-kill (process)
  "PROCESS を終了する。
プロセスにバッファが設定されている場合、それも kill-buffer する。"
  (set-process-filter process nil)
  (delete-process process)
  (if (process-buffer process)
      (kill-buffer (process-buffer process))))

(provide 'lookup-utils)

;;; lookup-utils.el ends here
