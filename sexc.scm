(import brev-separate fmt fmt-c tree (chicken string))

(define (unkebabify sym)
  (string->symbol
   (string-translate (symbol->string sym) #\- #\_)))

(define (loop)
  (let ((r (read)))
    (unless (eof-object? r)
      (cond ((eqv? (car r) 'define) (eval r))
            (#t
             (fmt #t
                  (c-expr
                   (tree-map
                    (fn
	             (case x
                       ((fn) '%fun)
                       ((var) '%var)
                       ((begin) '%begin)
	               ((pointer) '%pointer)
                       ((array) '%array)
                       (([]) 'vector-ref)
                       ((include) '%include)
	               ((cast) '%cast)
                       (else
                        (if (symbol? x)
                            (unkebabify x)
                            x))))
                    r)))
             (fmt #t "\n")))
      (loop))))

(loop)
