(define items
  '(
    ("メーリングリストのアドレス変更" "2001-06-19"
     ##(a #:href "lists/" "Lookup メーリングリスト")
     "のアドレス、及び subscribe/unsubscribe の仕方が変更になりました。")

    ("ndic sass support" "2000-11-11"
     "Yoishiro Okabe さんにより、ndic で sass をサポートするための"
     ##(a #:href "contrib/sdicf.el.sass.patch" "パッチ")
     "が公開されました。")

    ("Auto Lookup リリース" "2000-10-13"
     ##(a #:href "http://www.aist-nara.ac.jp/~masata-y/autolookup/index.html"
	  "Auto Lookup")
     "は、Lookup を使ってカーソル下の英単語を自動的に調べてミニバッファに"
     "結果を表示してくれるプログラムです。")

    ("FreePWING による JIS X 4081 版各種辞書" "2000-09-01"
     ##(a #:href "http://openlab.ring.gr.jp/edict/fpw/"
	  "FreePWING 辞書の配布ページ")
     "が電子辞書オープンラボに作成されました。")

    ("Unix で使える電子辞書情報" "2000-05-09"
     ##(a #:href "http://openlab.ring.gr.jp/edict/" "電子辞書オープンラボ")
     "から、"
     ##(a #:href "http://openlab.ring.gr.jp/edict/info.html"
	  "Unix などで使える電子辞書の情報")
     "が公開されています。これから電子辞書を買おうと思っている方、"
     "どの電子辞書を買おうか迷っている方はチェックしてみましょう。")
    ))

(define (news-item->html item)
  (let ((title (car item))
	(date (cadr item))
	(info (apply string-append
		     (map (lambda (x) (eval x (current-module)))
			  (cddr item)))))
    ##(li ##(p ##(font #:color "#3366cc" ##(b title))
	       " (" date ")" ##(br info)))))

(define news
  ##(ul (apply string-append (map news-item->html items))))
