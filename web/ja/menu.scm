(define contents
  '(("Information"
     ("Homepage"	"/lookup/")
     ("Download"	"/lookup/info/download.html")
     ("Screenshot"	"/lookup/info/screenshot.html")
     ("Mailing Lists"	"/lookup/lists/")
     )
    ("Development"
     ("Project Summary"	"http://sourceforge.net/projects/lookup")
     ("CVS Repository"	"http://sourceforge.net/cvs/?group_id=893")
     )
    ("Links"
     ("edict"		"/edict/")
     ("eblook"		"/edict/eblook/")
     ("NDTPD"		"http://www.sra.co.jp/people/m-kasahr/ndtpd/")
     ("FreePWING"	"http://www.sra.co.jp/people/m-kasahr/freepwing/")
     ("DICT Project"	"http://www.dict.org/")
     ("FreeDict Project" "http://www.freedict.de/")
     )
    ("Emacs"
     ("GNU Emacs"	"http://www.gnu.org/software/emacs/")
     ("Guile Emacs"	"http://gemacs.sourceforge.net/")
     ("XEmacs"		"http://www.xemacs.org/")
     ("NTEmacs"		"http://www.gnu.org/software/emacs/windows/ntemacs.html")
     ("Mule"		"http://www.m17n.org/mule/")
     ("Emacs/MacOS"	"http://www.csis.hku.hk/~choi/emacs/")
     )))

(define banners
  '(("Dictionary Links"
     ("Lookup"
      "/lookup/images/banner.png"
      "http://lookup.sourceforge.net/")
     ("FreePWING"
      "http://www.sra.co.jp/people/m-kasahr/freepwing/freepwing_b.png"
      "http://www.sra.co.jp/people/m-kasahr/freepwing/")
     ("edict"
      "http://openlab.ring.gr.jp/edict/edict_b.png"
      "http://openlab.ring.gr.jp/edict/")
     )
    ("Hosted by"
     ("RingServer"
      "/ringserver/buttons/RngSrvBtn.gif"
      "http://www.ring.gr.jp/")
     ("SourceForge"
      "http://sourceforge.net/sflogo.php?group_id=893&type=1"
      "http://sourceforge.net")
     )
    ("Created with"
     ("Happy Hacking Keyboard"
      "http://www.pfu.co.jp/images/hhkb.gif"
      "http://www.pfu.co.jp/hhkeyboard/")
     )))

(define (menu-item->html item)
  (define (subitem->html item)
    (let ((title (car item)) (url (cadr item)))
      (string-append
       "<br>"
       ##(a #:href url title))))
  (let ((heading (car item)) (items (cdr item)))
    ##(p ##(b heading ) (apply string-append (map subitem->html items)))))

(define (banner-item->html item)
  (define (subitem->html item)
    (let ((title (car item)) (src (cadr item)) (url (caddr item)))
      (string-append
       "<br>"
       ##(a #:href url
	   ##(img #:src src #:alt title #:width 88 #:height 31 #:border 0)))))
  (let ((heading (car item)) (items (cdr item)))
    ##(p heading (apply string-append (map subitem->html items)))))

(define menu
  (apply string-append
	 (append! (map menu-item->html contents)
		  (map banner-item->html banners))))
