(set! %load-path (cons "/home/kei/share/guile" %load-path))
(use-modules (text markup html))

(define title "Lookup - a Search Interface")

(define lookup-stable (href "Lookup 1.3" "http://download.sourceforge.net/lookup/lookup-1.3.tar.gz"))
(define lookup-unstable (href "Lookup 1.98" "DIST/beta/lookup-1.98.tar.gz"))

(define news
  '(
    ("ndic sass support" "2000-11-11"
     ("Yoishiro Okabe さんにより、ndic で sass をサポートするための"
      (href "パッチ" "contrib/sdicf.el.sass.patch")
      "が公開されました。"))

    ("Auto Lookup リリース" "2000-10-13"
     ((href "Auto Lookup"
	    "http://www.aist-nara.ac.jp/~masata-y/autolookup/index.html")
      "は、Lookup を使ってカーソル下の英単語を自動的に調べてミニバッファに"
      "結果を表示してくれるプログラムです。"))

    ("FreePWING による JIS X 4081 版各種辞書" "2000-09-01"
     ((href "FreePWING 辞書の配布ページ"
	    "http://openlab.ring.gr.jp/edict/fpw/")
      "が電子辞書オープンラボに作成されました。"))

    ("srd-fpw 1.1.1" "2000-08-25"
     ((href "srd-fpw" "http://openlab.ring.gr.jp/edict/srd-fpw/")
      "とは、小学館『ランダムハウス英語辞典』を EPWING 形式に変換するための"
      "スクリプトです。"))

    ("機能限定版「新英和・和英中辞典」" "2000-07-12"
     ("DeAGOSTINI『PC Success』第 24 号(1324円)の付録として、"
      "研究社「新英和・和英中辞典」の機能限定版が付いているそうです。"
      "後方一致検索が出来ませんが、Lookup でも利用出来るようです。"))

    ("mypaedia-fpw 1.4.1" "2000-07-14"
     ((href "mypaedia-fpw" "http://openlab.ring.gr.jp/edict/mypaedia-fpw/")
      "とは、平凡社の小型百科事典『マイペディア』を EPWING 形式に変換する"
      "ためのスクリプトです。DeAGOSTINI から発売されている『PC Success』の"
      "創刊号(500円)には、『マイペディア99』の機能限定版が附属しています。"))

    ("FreePWING 1.2.1" "2000-07-07"
     ((href "FreePWING" "http://www.sra.co.jp/people/m-kasahr/freepwing/")
      "とは、各種辞書データを EPWING 形式に変換するためのツールです。"
      "いくつかのフリーな辞書データも公開されています。"))

    ("wdic-fpw 1.1" "2000-06-24"
     ((href "wdic-fpw"
	    "http://member.nifty.ne.jp/~satomii/freepwing/indexj.html")
      "とは、フリーな通信用語集"
      "『" (href "通信用語の基礎知識" "http://www.wdic.org/") "』を"
      "EPWING 形式に変換するためのスクリプトです。"))

    ("Unix で使える電子辞書情報" "2000-05-09"
     ((href "電子辞書オープンラボ" "http://openlab.ring.gr.jp/edict/") "から、"
      (href "Unix などで使える電子辞書の情報"
	    "http://openlab.ring.gr.jp/edict/info.html")
      "が公開されています。これから電子辞書を買おうと思っている方、"
      "どの電子辞書を買おうか迷っている方はチェックしてみましょう。"))
    ))

(define body
  (string-append
   (h1 "What is Lookup")
   (indent
    (p "Lookup は Emacs エディタで利用できる辞書検索インターフェースです。"
       "市販の CD-ROM 辞書やネットワークの辞書サーバを始め、"
       "様々な情報源から簡単な操作と設定で辞書検索が行なえます。")

    (p "Lookup をこれから使われるという方は"
       (href "ユーザーズガイド" "guide/")
       "を参照して下さい。より詳しい情報を得るには"
       (href "ユーザーズマニュアル" "manual/")
       "を参照して下さい。更なる質問や情報のためには"
       (href "メーリングリスト" "lists/")
       "に加わって下さい。"))

   (h1 "Latest Release")
   (ul (li "Stable version: " lookup-stable ", " (href "eblook-1.3" "eblook/"))
       (li "Unstable version: " lookup-unstable)
       (li "Contribution: "
	   (href "Auto Lookup" "http://www.aist-nara.ac.jp/~masata-y/autolookup/index.html") ", "
	   (href "sass patch" "contrib/sdicf.el.sass.patch")))

   (h1 "Dictionary News")
   (ul (map-append (lambda (data)
		     (let ((title (car data))
			   (date (cadr data))
			   (info (map-append (lambda (x) (eval x (current-module)))
					     (caddr data))))
		       (li (p (font #:color "#3366cc" (b title))
			      " (" date ")" (br info)))))
		   news))
   (hr)
   (address
    "Last modified: $Date: 2000/11/19 23:59:52 $"
    "<br>Copyright (C) 2000 Keisuke Nishida &lt;knishida@ring.gr.jp&gt;"
    "<br>Graphics (C) 2000 Sumiya Sakoda")
   (p (href (img "http://www2.valinux.com/adserver.phtml?f_s=468x60&f_p=478"
		 #:alt "Member of the VA Affiliate Underground"
		 #:width 468 #:height 60 #:border 0)
	    "http://www2.valinux.com/adbouncer.phtml?f_s=468x60&f_p=478"))))

(load "menu.scm")

(print-html
 (table
  (tr #:valign "bottom"
   (td (img "/lookup/images/title.png" #:alt "Lookup"))
   (td "Language:"
       (href "English" "http://lookup.sourceforge.net/")
       (href "Japanese" "http://openlab.ring.gr.jp/lookup/"))))
 (table #:cellpadding "4"
  (tr #:valign "top"
   (td #:bgcolor "#cccccc" #:nowrap "" menu)
   (td body))))
