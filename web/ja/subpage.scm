
(define title (string-append "Lookup - " name " -"))

(define body
  (string-append
   (h2 subtitle)
   (if (defined? 'summary) summary "")
   (hr)
   subbody
   (hr)
   (address "Last modified: " date "<BR>")))

(load "menu.scm")

(html
 (table
  (tr #:valign "botton"
   (td (img "/lookup/images/title-mini.png" #:alt "Lookup"))
   (td "Language:"
       (href "English" "http://lookup.sourceforge.net/")
       (href "Japanese" "http://openlab.ring.gr.jp/lookup/"))))
 (table #:cellpadding "4"
  (tr #:valign "top"
   (td #:bgcolor "#cccccc" #:nowrap "" menu)
   (td body))))
