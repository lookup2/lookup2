(load "menu.scm")

(define copyright
  "Copyright (C) 2000 Keisuke Nishida &lt;knishida@ring.gr.jp&gt;")

(define page
  ##(html
     ##(head ##(title title))
     ##(body #:bgcolor "white"
	     ##(table
		##(tr #:valign "bottom"
		      ##(td ##(img #:src title-logo #:alt "Lookup"))
		      ##(td "Language:"
			    ##(a #:href "http://lookup.sourceforge.net/"
				 "English")
			    ##(a #:href "http://openlab.ring.gr.jp/lookup/"
				 "Japanese"))))
	     ##(table #:cellpadding "4"
		      ##(tr #:valign "top"
			    ##(td #:bgcolor "#cccccc" #:nowrap "" menu)
			    ##(td main
				  ##(hr)
				  ##(address "Last modified: " updated "<br>"
					     copyright)
				  footer))))))

(display page)
