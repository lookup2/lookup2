(load "../header.scm")

(define subtitle "Lookup Howto")
(define updated "$Date: 2001/03/24 22:11:01 $")

(define summary
  ##(p "このページには、Lookup の設定等について雑多に並べています。"
       "将来的には整理してマニュアルや FAQ に取り込まれる予定です。"))

(define subbody
  (string-append
   ##(ul
      ##(li ##(a #:href "gaiji.html" "外字導入ガイド"))
      ##(li ##(a #:href "os2-bitmap.html" "OS/2 で bitmap-muleのフォントを使う方法")))))

(load "../subpage.scm")
