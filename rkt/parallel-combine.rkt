#lang racket

(require
 (prefix-in ar: "./arity.rkt"))

(define (parallel-combine h f g)
  (let* ([n (ar:get-arity f)]
         [m (ar:get-arity g)]
         [l (ar:get-arity h)])
    
    ;; validate combine arity
    (when (and (number? l)
                (not (= 2 l)))
      (error "Combine fn arity missmatch"))

    ;; validate params
    (when (not (arity=? n m))
      (error "Incompatible arity length"))
    
    (define (the-combination . args)
      (when (not (arity-includes? n (length args)))
        (error "Called with wrong arity"))

      (h (apply f args)
         (apply g args)))
    
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
             "called with wrong arity")

  (check-exn exn:fail?
             (lambda ()
               (parallel-combine
                (lambda (z) z)
                (lambda (x) x)
                (lambda (y) y)))
             "arity missmatch in combine fn")
  
  (check-equal? ((parallel-combine
                  (lambda (x y)
                    (/ x y))
                  +
                  +)
                 2 2)
                1
                "allow at least arity"))
