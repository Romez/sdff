#lang racket

(require
 (prefix-in ar: "./arity.rkt"))

(define (compose f g)
  (let ([n (ar:get-arity f)]
        [m (ar:get-arity g)])
    (when (not (= n m))
      (error "Incompatable arity"))
    
    (define (the-composition . args)
      (when (not (= m (length args)))
        (error "Call compose arity missmatch"))
      
      (f (apply g args)))
    
    (ar:restrict-arity the-composition m)))

;; curried function (define (iterate n) (lambda (f) ...
(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))

(module+ test
  (require rackunit)
  
  (check-equal? ((compose (lambda (x) (list 'foo x))
                          (lambda (x) (list 'bar x)))
                 'z)
                '(foo (bar z))
                "Compose should work")

  (check-equal? (((iterate 3) add1) 5)
                8)

  (check-equal? (ar:get-arity
                 (compose (lambda (x) x)
                          (lambda (y) y)))
                1
                "compose arity")

  (check-exn exn:fail?
             (lambda ()
               (compose
                (lambda (c d) (+ c d))
                (lambda (a) a)))
             "Components have compatible arity")

  (check-exn exn:fail?
             (lambda ()
               ((compose
                 (lambda (c) (+ c 1))
                 (lambda (a b) (+ a b 1))) 1 2 3))
             "called with wrong nubmer of arguments"))

