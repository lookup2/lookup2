(define-public (tag-read ch port)
  (let* ((ls (read port))
	 (tag (let ((x (car ls)))
		(cond ((symbol? x) (symbol->string x))
		      (else (error "Invlaid tag:" x))))))
    (let loop ((ls (cdr ls)) (ks '()) (bs '()))
      (if (null? ls)
	  `(string-append ,(format #f "<~A" tag)
			  ,@(reverse! ks)
			  ">"
			  ,@(reverse! bs)
			  ,(format #f "</~A>\n" tag))
	  (let ((x (car ls)))
	    (if (keyword? x)
		(let ((k `(format #f " ~A=\"~A\""
				  ',(keyword->symbol x)
				  ,(cadr ls))))
		  (loop (cddr ls) (cons k ks) bs))
		(loop (cdr ls) ks (cons x bs))))))))