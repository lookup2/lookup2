(load "../header.scm")

(define subtitle "メーリングリスト一覧")
(define updated "$Date: 2001/03/24 22:11:01 $")
(define summary
  ##(p "Lookup に関連して、以下のいくつかのメーリングリストがあります。"))

(define subbody
  (string-append
   ##(dl
      ##(dt "lookup-ja@ring.gr.jp")
      ##(dd "Lookup に関する情報交換を行なうためのメーリングリストです。"
	    "参加を希望される方は、"
	    ##(a #:href "mailto:lookup-ja-request@ring.gr.jp"
		 "lookup-ja-request@ring.gr.jp")
	    "宛てに、本文に \"subscribe\" と書いたメールを送って下さい。")
      ##(ul
	 ##(li ##(a #:href "http://news.ring.gr.jp/news/openlab.lookup-ja/"
		    "アーカイブ"))
	 ##(li ##(a #:href "news://news.ring.gr.jp/ring.openlab.lookup-ja"
		    "ニュースグループ")))
      ##(br)
      ##(dt ##(a #:href "/openlab/edict/" "edict ML"))
      ##(dd "電子辞書一般について情報交換を行なうためのメーリングリストです。"
	    "Lookup に関係する情報もときおり流れます。")
      ##(ul
	 ##(li ##(a #:href "http://news.ring.gr.jp/news/openlab.edict/"
		    "アーカイブ"))
	 ##(li ##(a #:href "news://news.ring.gr.jp/ring.openlab.edict"
		    "ニュースグループ")))
      ##(br)
      ##(dt ##(a #:href "http://www.sra.co.jp/people/m-kasahr/ndtpd/" "NDTPD"))
      ##(dd "NDTPD のメーリングリストでも Lookup 関連の話題が出ることがあります。"
	    "特に市販の CD-ROM 辞書を利用している場合、NDTPD のメーリングリストにも"
	    "参加されることをお勧めします。"))))

(load "../subpage.scm")
