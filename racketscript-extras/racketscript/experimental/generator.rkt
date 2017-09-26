#lang racketscript/base

(require racket/dict
         racket/function
         syntax/parse/define
         syntax/parse
         syntax/id-table
         syntax/stx
         (for-syntax syntax/parse/define
                     racket/base
                     syntax/stx)
         "anf.rkt"
         "freshen.rkt")

(define (yield v)
  (error 'yield "Should be compiled to something else!"))

(define (lift-bindings stx)
  (define lexical-bindings (make-parameter #f))

  (define (save-bindings! b*)
    (stx-map (λ (i)
               (dict-set! (lexical-bindings) i #t))
             b*))

  (define default-value-stx #'(#%plain-app void))

  (define (lift stx)
    (syntax-parse stx
      #:literal-sets ((kernel-literals))
      [(#%plain-lambda formals body ...+)
       (parameterize ([lexical-bindings (make-free-id-table)])
         (with-syntax ([(lifted-body ...) (stx-map lift #'(body ...))]
                       [(collected-bindings ...) (map (λ (b)
                                                        #`((#,b) #,default-value-stx))
                                                      (dict-keys (lexical-bindings)))])
           (with-syntax ([new-body  (syntax-property #'(let-values (collected-bindings ...)
                                                         lifted-body ...)
                                                     'racketscript-generator-lifted-let-bindings
                                                     #t)])
             #`(#%plain-lambda formals new-body))))]
      [(let-values ([(id ...) bb] ...) body ...+)
       #:with (lifted-body ...) (stx-map lift #'(body ...))
       #:with (lifted-bb ...) (stx-map lift #'(bb ...))
       #:with ((fresh-id ...) ...) (map (λ (ids)
                                          (generate-temporaries (syntax-e ids)))
                                        (syntax-e #'((id ...) ...)))
       #:with (set!-bindings ...) (stx-map (syntax-parser
                                             [((id) bb) (lift #'(set! id bb))]
                                             #;[((id ...) bb)
                                                #:with (fresh-id ...) (generate-temporaries (syntax-e #'(id ...)))
                                                #'(let-values ([(fresh-id ...) bb])
                                                    (set! id fresh-id) ...)])
                                           #'([(id ...) lifted-bb] ...))

       (save-bindings! #'(id ... ...))
       #`(let-values () set!-bindings ... lifted-body ...)]
      [(letrec-values ([(id ...) bb] ...) body ...+)
       (error 'normalize "letrec unimplemented")]
      [(with-continuation-mark key val result)
       (error 'normalize "w-c-m unimplemented")]
      [(if pred t-branch f-branch)
       #`(if #,(lift #'pred)
             #,(lift #'t-branch)
             #,(lift #'f-branch))]
      [(case-lambda . clauses)
       (error 'normalize "case-lambda unimplemented")]
      [(#%plain-app lam . args)
       #:with lifted-lam (lift #'lam)
       #:with lifted-args (stx-map lift #'args)
       #`(#%plain-app lifted-lam . lifted-args)]
      [(begin e ...+)
       ;#:with (lifted-e ...) (stx-map lift #'(e ...))
       #;#`(begin lifted-e ...)
       (error 'lift-bindings "begin should be expanted to let-values")]
      #;[(begin0 e0 e ...)
       #:with lifted-e0 (lift #'e0)
       #:with (lifted-e ...) (stx-map lift #'(e ...))
       #`(begin0 lifted-e0 lifted-e ...)]
      [(set! id expr)
       #:with lifted-expr (lift #'expr)
       ;#:with (temp) (generate-temporaries (list #'id))
       #`(set! id lifted-expr)
       #;(cond
           [(stx-terminal? #'lifted-expr) #`(set! id lifted-expr)]
           [else
            (save-bindings! #'(temp))
            #`(let-values ()
                (set! temp lifted-expr)
                (set! id temp))])]
      [(#%expression expr)
       #:with lifted-expr (lift #'expr)
       #`(#%expression lifted-expr)]
      [v #'v]))

  (parameterize ([lexical-bindings (make-free-id-table)])
    (lift stx)))

(define (lift-empty-lets stx)
  (define (lift* stx*)
    (apply append
           (reverse
            (for/fold ([results '()])
                      ([stx* (syntax->list stx*)])
              (syntax-parse stx*
                #:literal-sets ((kernel-literals))
                [(let-values () body ...)
                 #:with (lifted-body ...) (lift* #'(body ...))
                 (cons (syntax->list #'(lifted-body ...)) results)]
                [(if (let-values () pred-body ...) t-branch f-branch)
                 #:with (lifted-pred-body ... lifted-pred-val) (lift* #'(pred-body ...))
                 #:with lifted-if-expr (lift #'(if lifted-pred-val t-branch f-branch))
                 (cons (syntax->list #'(lifted-pred-body ... lifted-if-expr)) results)]
                [(set! id (let-values () body ...))
                 #:with (lifted-body ... val) (lift* #'(body ...))
                 (cons (syntax->list #'(lifted-body ... (set! id val)))
                       results)]
                [_
                 (define lifted-stx (lift stx*))
                 (cons (if (syntax? lifted-stx)
                           (list lifted-stx)
                           lifted-stx)
                       results)])))))

  (define (lift stx)
    (syntax-parse stx
      #:literal-sets ((kernel-literals))
      [(#%plain-lambda formals body ...+)
       #:with (lifted-body ...) (lift* #'(body ...))
       #'(#%plain-lambda formals lifted-body ...)]
      [(let-values clauses body ...+)
       #:with (lifted-body ...) (lift* #'(body ...))
       #'(let-values clauses lifted-body ...)]
      [(letrec-values ([(id ...) bb] ...) body ...+)
       (error 'normalize "letrec unimplemented")]
      [(with-continuation-mark key val result)
       (error 'normalize "w-c-m unimplemented")]
      [(if pred t-branch f-branch)
       #:with lifted-pred (lift #'pred)
       #:with lifted-t-branch (lift #'t-branch)
       #:with lifted-f-branch (lift #'f-branch)
       #'(if lifted-pred lifted-t-branch lifted-f-branch)]
      [(#%expression expr)
       #:with lifted-expr (lift #'expr)
       #'(#%expression lifted-expr)]
      [(case-lambda . clauses)
       (error 'normalize "case-lambda unimplemented")]
      [v #;{Anf no lifting required} #'v]))

  (lift stx))

#;(define-syntax-parser function*
  [(_ (f0 ...) body ...) #'#f]
  [(_ (f0 ... . fv) body ...) #'#f]
  [(_ fv) #'#f])


(module+ test
  (require rackunit racket/pretty)

  #;(test-case "Lift bindings"
    (define prog (expand #'(λ ()
                             (let ([a 1]
                                   [b 2])
                               (+ 1 (let ([c 3])
                                      (set! a 4)
                                      (+ (+ c a) (begin (set! b 5) b))))))))

    (pretty-print (syntax->datum (lift-bindings prog))))

  (test-case "Lift bindings with freshen"
    (define prog (freshen (expand #'(λ ()
                                      (let ([a 1]
                                            [b 2]
                                            [d 0])
                                        (+ 1 d (let ([d 3])
                                                 (set! a 4)
                                                 (+ (+ c a) d (begin (set! b 5) b)))))))))

    (define prog2 (normalize prog identity))
    (pretty-print (syntax->datum prog))
    ;; (pretty-print (syntax->datum prog2))
    (pretty-print (syntax->datum   (lift-bindings prog2)))
    #;(pretty-print (syntax->datum  (normalize prog identity)))
    (pretty-print (syntax->datum  (lift-empty-lets (lift-bindings prog2))))
    #;(pretty-print (syntax->datum (expand (lift-bindings (lift-bindings (normalize prog identity))))))
    #;(pretty-print (syntax->datum prog2))
    #;(pretty-print (syntax->datum (lift-bindings prog2)))
    #;(pretty-print (syntax->datum (normalize (lift-bindings prog) identity))))

  #;(test-case "Generator of a sequence of numbers"
    (define prog #'(define gen-range (function* (n)
                                       (let loop ([i 0])
                                         (cond
                                           [(< i n) (yield i)
                                                    (loop (add1 i))]
                                           [else n])))))

    (pretty-print (syntax->datum (expand prog)))))
