#lang racket

(require
 (prefix-in ar: "./arity.rkt"))

(define (parallel-combine h f g)
  (let* ([n (ar:get-arity f)]
         [m (ar:get-arity g)])
    (when (not (= n m))
      (error "Incompatible arity"))
    
    (define (the-combination . args)
      (when (not (= (length args) n m))
        (error "Called with wrong arity"))
      (h (apply f args) (apply g args)))
    
    (ar:restrict-arity the-combination m)))

(module+ test
  (require rackunit)
  
  (check-equal? ((parallel-combine list
                                   (lambda (x y z) (list 'foo x y z))
                                   (lambda (u v w) (list 'bar u v w)))
                 'a 'b 'c)
                '((foo a b c) (bar a b c)))

  (check-equal? (ar:get-arity
                 (parallel-combine list
                                   (lambda (x y z) (list 'foo x y z))
                                   (lambda (u v w) (list 'bar u v w))))
                3
                "advertises correct arity")

  (check-exn exn:fail?
             (lambda ()
               (parallel-combine
                list
                (lambda (a) a)
                (lambda (a b) (+ a b))))
             "incompatible arity")
  (check-exn exn:fail?
             (lambda ()
               ((parallel-combine
                 (lambda (x y) (+ x y))
                 (lambda (a) (+ a 1))
                 (lambda (a) (+ a 1)))
                1 2))
             "called with wrong arity"))
