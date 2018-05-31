(module csdoc ()
  (import chicken scheme)
  
  (begin-for-syntax
    (import chicken scheme)
    (use srfi-69)
    
    ; define storage for all documentation nodes
    (define root '())
    (define nodes (make-hash-table #:test equal?))
    
    ; helper functions
    (define (as-node name)
      (if (symbol? name)
        (append root name)
        name
      )
    )
    
    ; define handlers for the procedure `document`, handling the creation of new documentation nodes.
    (define document-handlers (make-hash-table #:test eqv?))
    (hash-table-set! document-handlers 'procedure (lambda (rest)
      (let* (
        (formals (car rest))
        (body (cdr rest))
        
        (name (car formals))
      )
        (hash-table-set! nodes (as-node name) body)
      )
    ))
    
    ; workaround for issue #1465
    (define (get-keyword kw args . default) (let (
      (tail (memq kw args))
    )
      (if (and tail (not (null? (cdr tail))))
        (cadr tail)
        (if (null? default)
          #f
          ((car default))
        )
      )
    ))
  )
  
  (export document)
  (define-syntax document (er-macro-transformer (lambda (exp rename compare)
    (let* (
      (kind (cadr exp))
      (rest (cddr exp))
      (handler (hash-table-ref document-handlers kind))
    )
      (handler rest)
    )
    '(void)
  )))
  
  ; TODO: will probably remove this later in favor of other ideas
  (export docs-for)
  (define-syntax docs-for (er-macro-transformer (lambda (exp rename compare)
    `',(hash-table-ref/default nodes (cadr exp) #f)
  )))
)
