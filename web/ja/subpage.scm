
(define title (format #f "Lookup - ~A -" subtitle))
(define title-logo "/lookup/images/title-mini.png")
(define footer "")

(define main
  (string-append
   ##(h2 subtitle)
   summary
   ##(hr)
   subbody))

(load "template.scm")
