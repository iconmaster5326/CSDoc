(module csdoc ()
  (import chicken scheme)
  
  (begin-for-syntax
    (import chicken scheme)
    (use srfi-1 srfi-69)
    
    ;; define storage for all documentation nodes
   
    ; root: The current node in the documentation tree we're working in
    (define root '())
    
    ; nodes: A map of full paths to thier documentation nodes.
    ; a node is stored as an a-list.
    (define nodes (make-hash-table #:test equal?))
    
    ;; helper functions
    ; returns the full path of a node specifier.
    (define (as-node name)
      (if (symbol? name)
        (append root name)
        name
      )
    )
    
    ; Converts a list of keyword-value pairs into an alist. The key is the keyword, of #f for the unkeyworded args.
    (define (keywords->alist args)
      (let* (
        (result '((#f)))
        (rest (car result))
        (kw #f)
      )
        (for-each (lambda (arg)
          (if kw
            (set! result (cons (cons kw arg) result))
            (if (keyword? arg)
               (set! kw arg)
               (set-cdr! rest (cons arg rest))
            )
          )
        ) args)
        (set-cdr! rest (reverse! (cdr rest)))
        result
      )
    )
    
    ;; define handlers for the procedure `document`, handling the creation of new documentation nodes.
    (define document-handlers (make-hash-table #:test eqv?))
    
    ; procedure: a procedure.
    ; Properties include:
    ;   name: Its name. A shortcut for (car formals), really.
    ;   formals: A lambda-list, like the one you give to define to make a procedure.
    ;   desc: A description of the function.
    (hash-table-set! document-handlers 'procedure (lambda (rest)
      (let* (
        (formals (car rest))
        (body (cdr rest))
        
        (raw-name (car formals))
        (name (if (symbol? raw-name) raw-name (last raw-name)))
        (args (cdr formals))
        
        (properties (keywords->alist body))
        (positional-args (cdr (assq #f properties)))
        
        (node `(procedure
          (name . ,name)
          (formals . ,formals)
          (desc . ,(car positional-args))
        ))
      )
        (hash-table-set! nodes (as-node raw-name) node)
      )
    ))
    
    ;; workaround for issue #1465
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
