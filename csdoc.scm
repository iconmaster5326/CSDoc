(module csdoc ()
  (import chicken scheme)
  (use srfi-69)
  
  (begin-for-syntax
    (import chicken scheme)
    (use srfi-69)
    
    (define root '())
    (define nodes (make-hash-table #:test equal?))
  )
  
  (export document)
  (define-syntax document (er-macro-transformer (lambda (exp rename compare)
    (let* (
      (kind (cadr exp))
      (rest (cddr exp))
    )
      (hash-table-set! nodes (car rest) (cdr rest))
    )
    '(void)
  )))
 
  (export docs-for)
  (define-syntax docs-for (er-macro-transformer (lambda (exp rename compare)
    `',(hash-table-ref/default nodes (cadr exp) #f)
  )))
)
