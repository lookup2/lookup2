(load "header.scm")

(define title "Lookup - a Search Interface")
(define title-logo "/lookup/images/title.png")
(define updated "$Date: 2001/07/10 04:59:07 $")

(define lookup-stable
  ##(a #:href "http://download.sourceforge.net/lookup/lookup-1.3.tar.gz"
      "Lookup 1.3"))
(define lookup-unstable
  ##(a #:href "DIST/beta/lookup-1.99.1.tar.gz" "Lookup 1.99.1"))

(load "news.scm")

(define main
  (string-append
   ##(h1 "What is Lookup")
   ##(blockquote
     ##(p "Lookup は Emacs エディタで利用できる辞書検索インターフェースです。"
	 "市販の CD-ROM 辞書やネットワークの辞書サーバを始め、"
	 "様々な情報源から簡単な操作と設定で辞書検索が行なえます。"))

   ##(h1 "Latest Release")
   ##(unless
     ##(li "Stable version: " lookup-stable ", " ##(a #:href "/edict/eblook/" "eblook-1.3"))
     ##(li "Unstable version: " lookup-unstable)
     ##(li "Contribution: "
	  ##(a #:href "http://www.aist-nara.ac.jp/~masata-y/autolookup/index.html" "Auto Lookup") ", "
	  ##(a #:href "contrib/sdicf.el.sass.patch" "sass patch")))

   ##(h1 "Dictionary News")
   news))

(define footer
  ##(p ##(a #:href "http://www2.valinux.com/adbouncer.phtml?f_s=468x60&f_p=478"
	    ##(img #:src "http://www2.valinux.com/adserver.phtml?f_s=468x60&f_p=478"
		   #:alt "Member of the VA Affiliate Underground"
		   #:width 468 #:height 60 #:border 0))))

(load "template.scm")
