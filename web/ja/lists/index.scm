(set! %load-path (cons "../.." %load-path))
(use-modules (htmlgen))

(define name "Mailing Lists")
(define subtitle "メーリングリスト一覧")
(define date "$Date: 2000/07/16 20:49:08 $")

(define summary
  (p "Lookup に関連して、以下のいくつかのメーリングリストがあります。"))

(define subbody
  (string-append
   (dl
    (dt "lookup-ja@ring.gr.jp")
    (dd "Lookup に関する情報交換を行なうためのメーリングリストです。"
	"参加を希望される方は、"
	(href "lookup-ja-request@ring.gr.jp"
	      "mailto:lookup-ja-request@ring.gr.jp")
	"宛てに、本文に \"subscribe\" と書いたメールを送って下さい。")
    (ul
     (li (href "アーカイブ" "http://news.ring.gr.jp/news/openlab.lookup-ja/"))
     (li (href "ニュースグループ" "news://news.ring.gr.jp/ring.openlab.lookup-ja")))
    (br)
    (dt (href "edict ML" "/openlab/edict/"))
    (dd "電子辞書一般について情報交換を行なうためのメーリングリストです。"
	"Lookup に関係する情報もときおり流れます。")
    (ul
     (li (href "アーカイブ" "http://news.ring.gr.jp/news/openlab.edict/"))
     (li (href "ニュースグループ" "news://news.ring.gr.jp/ring.openlab.edict")))
    (br)
    (dt (href "NDTPD" "http://www.sra.co.jp/people/m-kasahr/ndtpd/"))
    (dd "NDTPD のメーリングリストでも Lookup 関連の話題が出ることがあります。"
	"特に市販の CD-ROM 辞書を利用している場合、NDTPD のメーリングリストにも"
	"参加されることをお勧めします。"))))

(load "../subpage.scm")
