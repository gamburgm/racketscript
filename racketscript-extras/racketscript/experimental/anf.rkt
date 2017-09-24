#lang racket/base

(require racket/match
         racket/function
         syntax/stx
         syntax/parse
         (for-syntax syntax/parse))

(provide normalize)

(define (stx-terminal? stx)
  (or (not (stx-pair? stx))
      (syntax-parse stx
        [(quote d) #t]
        [_ #f])))

(define (normalize* stx* k)
  (match stx*
    ['() (k stx*)]
    [(cons hd tl)
     (normalize-name hd (λ (k-hd)
                          (normalize* tl (λ (k-tl)
                                           (k (cons k-hd k-tl))))))]))

(define (normalize-name stx k)
  (normalize stx
             (λ (stx-exp)
               (cond
                 [(stx-terminal? stx-exp) (k stx-exp)]
                 [else
                  (define temp (car (generate-temporaries #'(temp))))
                  #`(let-values ([(#,temp) #,stx-exp])
                      #,(k temp))]))))

(define (normalize stx k)
  (syntax-parse stx
    #:literal-sets ((kernel-literals))
    [(let-values () body ...+)
     (normalize* (syntax-e #'(body ...))
                 (λ (k-body)
                   #`(let-values () #,@(k k-body))))]
    [(let-values ([(id ...) bb] ...) body ...+)
     (normalize*
      (syntax-e #'(bb ...))
      (λ (k-bb)
        (with-syntax ([(bb* ...) k-bb])
          (k #`(let-values ([(id ...) bb*] ...)
                 #,@(stx-map (λ (b)
                               (normalize b identity))
                             #'(body ...)))))))
     #;(normalize #'bb
                (λ (k-bb)
                  #`(let-values ([(id ...) #,k-bb])
                      #,(normalize #`(let-values clauses
                                       body ...)
                                   k))))]

    [(letrec-values ([(id ...) bb] ...) body ...+)
     (error 'normalize "letrec unimplemented")]
    [(with-continuation-mark key val result)
     (error 'normalize "w-c-m unimplemented")]
    [(if pred t-branch f-branch)
     (normalize-name #'pred
                     (λ (apred)
                       (k #`(if #,apred
                                #,(normalize #'t-branch identity)
                                #,(normalize #'f-branch identity)))))]
    [(#%plain-lambda formals body ...+)
     (k #`(#%plain-lambda formals
                          #,(normalize* (syntax->list #'(body ...)) identity)))]
    [(case-lambda . clauses)
     (error 'normalize "case-lambda unimplemented")]
    [(#%plain-app lam . args)
     (normalize-name
      #'lam
      (λ (k-lam)
        (normalize* (syntax->list #'args)
                    (λ (k-args)
                      (k #`(#%plain-app #,k-lam #,@k-args))))))]
    [(begin e ...+)
     (k #`(begin
            #,@(stx-map (λ (t) (normalize t identity)) #'(e ...))))]
    [(begin0 e0 e ...)
     #`(begin0
           #,@(stx-map (λ (t) (normalize t identity)) #'(e0 e ...)))]
    [(set! id expr)
     (normalize-name #'expr (λ (k-expr)
                              #`(set! id #,k-expr)))]
    [(quote d) (k #'(quote d))]
    [(#%top . v) (k #'(#%top . v))]
    [(#%expression expr) (normalize #'expr (λ (aexpr)
                                             (k #`(#%expression #,aexpr))))]
    [v #:when (stx-terminal? #'v) (k #'v)]
    [v (error 'anf "Unexpected syntax term: ~a" #'v)]))

#|
(define prog
  #'(#%plain-lambda
     ()
     (let-values (((a1) '1) ((b2) '2) ((d3) '0))
       (#%plain-app
        +
        '1
        d3
        (let-values (((d4) '3))
          (set! a1 '4)
          (#%plain-app + (#%plain-app + (#%top . c) a1) d4 (begin (set! b2 '5) b2)))))))
(require racket/pretty)
(pretty-print (syntax->datum prog))
(pretty-print
 (syntax->datum (normalize prog identity)))
|#
#;(module+ test
  (require racket/pretty
           rackunit)

  (test-case "Normalize application"
    (define prog (expand #`(+ (/ 1 2) (/ 3 4))))

    (pretty-print
     (syntax->datum (normalize prog identity))))

  (test-case "Normalize application"
    (define prog (expand #'(+ (/ 1 2) (/ 3 4) (- 5 (* 4 5)))))

    (pretty-print
     (syntax->datum (normalize prog identity))))

  (test-case "Normalize let-values"
    (define prog (expand #'(let-values ([(a) (+ (/ 4 2) (+ 2 3))]
                                        [(b) (- 2)])
                             (* a b))))

    (pretty-print
     (syntax->datum (normalize prog identity))))

  (test-case "Normalize if expressions"
    (define prog (expand #'(let-values ([(a) (+ 2 3)]
                                        [(b) (sqr 2)])
                             (if (zero? a)
                                 (sqr (* a b))
                                 (* (/ a b) 2)))))

      (pretty-print
       (syntax->datum (normalize prog (λ (x) x)))))


  (test-case "begin expressions"
    (define prog (expand #'(begin (+ (/ 1 2) (* 3 4))
                                  (+ (/ 6 7) (* 8 9)))))
    (pretty-print
     (syntax->datum (normalize prog (λ (x) x)))))

  (test-case "set!"
    (define prog (expand #'(let ([a 1])
                             (set! a (+ a 1)))))
    (pretty-print
     (syntax->datum (normalize prog (λ (x) x)))))
  )
