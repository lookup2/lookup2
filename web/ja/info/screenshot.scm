(set! %load-path (cons "../.." %load-path))
(use-modules (htmlgen))

(define name "Screenshot")
(define subtitle "スクリーンショット")
(define date "$Date: 2000/07/16 20:49:08 $")

(define subbody
  (string-append
   (img "jitenban97.gif")
   (p "アスキー出版"
      (href "『辞・典・盤97』" "http://www.ascii.co.jp/pb/jitenban/")
      "より検索。")
   (hr)
   (img "aiai.jpg")
   (p "平凡社『マイペディア99』より検索")))

(load "../subpage.scm")
