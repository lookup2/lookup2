(load "../header.scm")

(define subtitle "外字導入ガイド")
(define updated "$Date: 2001/03/24 22:11:01 $")
(define summary "")

(define subbody
  ##(ol
     ##(li (href "ftp://ftp.m17n.org/pub/mule/apel/")
	   "から最新版の APEL を取ってくる。"
	   ##(p "make install でインストール。"))

     ##(li (href "ftp://ftp.jpl.org/pub/elisp/bitmap/")
	   "から最新版の bitmap-mule を取ってくる。"
	   ##(p "make install でインストール。")
	   ##(p "font/Makefile の FONTDIR を適当に修正し、"
		"cd font; make install でフォントをインストール。"))

     ##(li "~/.lookup に次のように書いておく。"
	   ##(pre "(setq lookup-use-bitmap t)"))))

(load "../subpage.scm")
