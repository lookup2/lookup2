(load "../header.scm")

(define subtitle "Lookup TIPS")
(define updated "$Date: 2001/03/24 22:11:01 $")
(define summary ##(p "Lookup を使う上でお得な情報。"))

(define subbody
  (string-append
   ##(ul
      ##(li "最近の Emacs だと、<tt>lookup-pattern</tt>"
	    "で表示されるデフォルトの文字列を <tt>M-n</tt>"
	    "で編集出来るようになる。ちょっと修正したいときに便利。")
      ##(li "変数 <tt>gc-cons-threshold</tt> の値をデフォルトよりも"
	    "大きくしておくと(10倍くらい)、GC の頻度が減ってプログラムの"
	    "読み込み・実行速度が心なし早くなる。")
      ##(li "EPWUTIL の catdump を使って複数の EPWING 辞書を一つのディレクトリに"
	    "まとめると、eblook のプロセスが一つで済むのでシステムの負荷が"
	    "気持ちだけ減るかも。"))))

(load "../subpage.scm")
