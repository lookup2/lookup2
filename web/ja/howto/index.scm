(set! %load-path (cons "../.." %load-path))
(use-modules (htmlgen))

(define name "Howto")
(define subtitle "Lookup Howto")
(define date "$Date: 2000/07/16 20:49:08 $")

(define summary
  (p "このページには、Lookup の設定等について雑多に並べています。"
     "将来的には整理してマニュアルや FAQ に取り込まれる予定です。"))

(define subbody
  (string-append
   (ul
    (li (href "外字導入ガイド" "gaiji.html"))
    (li (href "OS/2 で bitmap-muleのフォントを使う方法" "os2-bitmap.html")))))

(load "../subpage.scm")
