(load "../header.scm")

(define subtitle "スクリーンショット")
(define updated "$Date: 2001/03/24 22:11:01 $")
(define summary "")

(define subbody
  (string-append
   ##(img #:src "jitenban97.gif")
   ##(p "アスキー出版"
	##(a #:href "http://www.ascii.co.jp/pb/jitenban/" "『辞・典・盤97』")
	"より検索。")
   ##(hr)
   ##(img #:src "aiai.jpg")
   ##(p "平凡社『マイペディア99』より検索")))

(load "../subpage.scm")
