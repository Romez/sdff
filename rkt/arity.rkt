#lang racket

(provide (all-defined-out))

(define arity-table (make-hash))

(define (restrict-arity proc nargs)
  (hash-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-ref arity-table proc #f)
      (let ([a (procedure-arity proc)])
        (if (list? a)
            (raise "Unhandled arity")
            a))))

(module+ test
  (require rackunit)
  
  (check-equal?
   (get-arity (lambda (x) x)) 1)
  
  (check-equal?
   (get-arity (lambda (x y) (+ x y))) 2)
  )
