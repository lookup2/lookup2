(set! %load-path (cons "../.." %load-path))
(use-modules (htmlgen))

(define name "OS/2 Bitmap")
(define subtitle "OS/2 で bitmap-muleのフォントを使う方法")
(define date "$Date: 2000/07/16 20:49:08 $")

(define summary (p "by Masaru Nomiya &lt;nomiya@pp.iij4u.or.jp&gt;"))

(define subbody
  (string-append
   (ol
    (li "bitmp-mule8.2.tar.gz を展開")
    (li "cd bitmap-mule-8.2")
    (li "make install"
	(p "インストール時に"
	   (pre "While compiling toplevel forms in file g:/bitmap-mule-8.2/gnus-bitmap.el:\n"
		"!! End of file during parsing")
	   "というメッセージが表示されると思いますが、気にすることはないようです。"))
    (li "mkfontdirを実行（fonts.dirを作成しておく為）"
	(p "../X11/lib/fonts/miscのようにフォントが多くあるところでは、"
	   "6. に示す fonts.dir の編集作業が煩雑になりますので、"
	   "pcf.gz 形式ファイルと bdf 形式ファイルだけのディレクトリを"
	   "利用するのが良いでしょう。小生の場合は、"
	   "bitmap-mule-8.2/font で実行しました。"))
    (li "bdf2mfn で全ての bdf 形式フォントを mfn 形式フォントに変換")
    (li "fonts.dir を編集"
	(p "fonts.dirの中味は、"
	   (pre "etl7x14-bitmap.pcf.gz -etl-fixed-medium-r-normal--14-105-100-100-m-70-bitmap.7x14-0\netc.")
	   "というように"
	   (pre "pcf.gz形式フォント名　ＸＬＦＤ形式フォント名")
	   "となっていますが、これを"
	   (pre "ＸＬＦＤ形式フォント名　ドライブ・ディレクトリを含むmfn形式ファイル名")
	   "つまり、"
	   (pre "-etl-fixed-medium-r-normal--14-105-100-100-m-70-bitmap.7x14-0 g:/bitmap-mule-8.2/font/etl7x14-bitmap.mfn")
	   "といった具合に書き換える。その後、ファイル名を FONTSET.OS2"
	   "としてこれをHOMEに入れる。")))

   (p "尚、mfn形式ファイルを用いるのは、emacs-20.2 for OS/2の制約からで、"
      "全ての場合に当て嵌るものではありません（詳細は、お使いのEmacs"
      "に付属のReadmeを参照して下さい）。")))

(load "../subpage.scm")
