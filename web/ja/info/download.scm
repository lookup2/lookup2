(load "../header.scm")

(define subtitle "ダウンロードサイト一覧")
(define updated "$Date: 2001/03/24 22:11:01 $")

(define summary
  ##(p "Lookup 関連のアーカイブは以下のところにミラーされています。"
       "お近くのサイトを選んでダウンロードして下さい。"))

(define subbody
  (string-append
   ##(h3 "Sourceforge")
   ##(ul
      ##(li (href "http://download.sourceforge.net/lookup/") "(lookup-1.x のみ)"))

   ##(h3 "Ring Servers")
   ##(ul
      ##(li (href "ftp://core.ring.gr.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.etl.go.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.asahi-net.or.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.crl.go.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.astem.or.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.jah.ne.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.nacsis.ac.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.exp.fujixerox.co.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.so-net.ne.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.ip-kyoto.ad.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.iwate-pu.ac.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.shibaura-it.ac.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.ocn.ad.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.htcn.ne.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.omp.ad.jp/pub/text/elisp/lookup/"))
      ##(li (href "ftp://ring.jec.ad.jp/pub/text/elisp/lookup/"))
      )))

(load "../subpage.scm")
