
(define (submenu heading . body) (p (b heading) (apply string-append body)))
(define (item title url) (string-append "<br>" (href title url)))

(define menu
  (string-append
   (submenu "Information"
    (item "Homepage" "/lookup/")
    (item "Download" "/lookup/info/download.html")
    (item "Screenshot" "/lookup/info/screenshot.html")
    (item "Mailing Lists" "/lookup/lists/")
    )
   (submenu "Development"
    (item "Project Summary" "http://sourceforge.net/projects/lookup")
    (item "CVS Repository" "http://sourceforge.net/cvs/?group_id=893")
    )
   (submenu "Links"
    (item "edict" "/edict/")
    (item "eblook" "/lookup/eblook/")
    (item "NDTPD" "http://www.sra.co.jp/people/m-kasahr/ndtpd/")
    (item "FreePWING" "http://www.sra.co.jp/people/m-kasahr/freepwing/")
    (item "DICT Project" "http://www.dict.org/")
    (item "FreeDict Project" "http://www.freedict.de/")
    )
   (submenu "Emacs"
    (item "GNU Emacs" "http://www.gnu.org/software/emacs/")
    (item "Guile Emacs" "http://gemacs.sourceforge.net/")
    (item "XEmacs" "http://www.xemacs.org/")
    (item "NTEmacs" "http://www.gnu.org/software/emacs/windows/ntemacs.html")
    (item "Mule" "http://www.m17n.org/mule/")
    (item "Emacs/MacOS" "http://www.csis.hku.hk/~choi/emacs/")
    )

   (p "Dictionary Links"
      (br (href (img "/lookup/images/banner.png"
		     #:alt "Lookup" #:width 88 #:height 33 #:border 0)
		"http://lookup.sourceforge.net/") "<br>"
	  (href (img "http://www.sra.co.jp/people/m-kasahr/freepwing/freepwing_b.png"
		     #:alt "FreePWING" #:width 88 #:height 31 #:border 0)
		"http://www.sra.co.jp/people/m-kasahr/freepwing/") "<br>"
	  (href (img "http://openlab.ring.gr.jp/edict/edict_b.png"
		     #:alt "edict" #:width 88 #:height 31 #:border 0)
		"http://openlab.ring.gr.jp/edict/")))

   (p "Hosted by"
      (br (href (img "/ringserver/buttons/RngSrvBtn.gif"
		     #:alt "RingServer" #:width 88 #:height 31 #:border 0)
		"http://www.ring.gr.jp/") "<br>"
	  (href (img "http://sourceforge.net/sflogo.php?group_id=893&type=1"
		     #:alt "SourceForge" #:width 88 #:height 31 #:border 0)
		"http://sourceforge.net")))

   (p "Created with"
      (br (href (img "http://www.pfu.co.jp/images/hhkb.gif"
		     #:alt "Happy Hacking Keyboard"
		     #:width 88 #:height 31 #:border 0)
		"http://www.pfu.co.jp/hhkeyboard/")))))
