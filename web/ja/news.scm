(define items
  '(
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

    ("srd-fpw 1.1.1" "2000-08-25"
     ##(a #:href "http://openlab.ring.gr.jp/edict/srd-fpw/" "srd-fpw")
     "とは、小学館『ランダムハウス英語辞典』を EPWING 形式に変換するための"
     "スクリプトです。")

    ("機能限定版「新英和・和英中辞典」" "2000-07-12"
     "DeAGOSTINI『PC Success』第 24 号(1324円)の付録として、"
     "研究社「新英和・和英中辞典」の機能限定版が付いているそうです。"
     "後方一致検索が出来ませんが、Lookup でも利用出来るようです。")

    ("mypaedia-fpw 1.4.1" "2000-07-14"
     ##(a #:href "http://openlab.ring.gr.jp/edict/mypaedia-fpw/"
	  "mypaedia-fpw")
     "とは、平凡社の小型百科事典『マイペディア』を EPWING 形式に変換する"
     "ためのスクリプトです。DeAGOSTINI から発売されている『PC Success』の"
     "創刊号(500円)には、『マイペディア99』の機能限定版が附属しています。")

    ("FreePWING 1.2.1" "2000-07-07"
     ##(a #:href "http://www.sra.co.jp/people/m-kasahr/freepwing/" "FreePWING")
     "とは、各種辞書データを EPWING 形式に変換するためのツールです。"
     "いくつかのフリーな辞書データも公開されています。")

    ("wdic-fpw 1.1" "2000-06-24"
     ##(a #:href "http://member.nifty.ne.jp/~satomii/freepwing/indexj.html"
	  "wdic-fpw")
     "とは、フリーな通信用語集"
     "『" ##(a #:href "http://www.wdic.org/" "通信用語の基礎知識") "』を"
     "EPWING 形式に変換するためのスクリプトです。")

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
