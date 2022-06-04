#lang racket

(require rackunit)

(define (compose f g)
  (lambda args
    (f (apply g args))))

(module+ test 
  (check-equal? ((compose (lambda (x) (list 'foo x))
                          (lambda (x) (list 'bar x)))
                 'z)
                '(foo (bar z))
                "Compose should work")
) 

#;
(define (compose f g)
  (let ([n (ar:get-arity f)]
        [m (ar:get-arity g)])
    (when (not (= 1 n))
      (error "Arity missmatch"))
    (ar:restrict-arity
     (lambda args
       (when (not (= m (length args)))
         (error "Call compose arity missmatch"))
       (f (apply g args)))
     m)))



#;
(check-eq? (ar:get-arity (compose
                          (lambda (c) (+ c 1))
                          (lambda (a b) (+ a b 1))))
           2
           "Check compose arity")

#;
(check-exn
 exn:fail?
 (lambda ()
   (compose
    (lambda (c d) (+ c d 1))
    (lambda (a b) (+ a b 1))))
 "Components have compatible arity")

#;
(check-exn
 exn:fail?
 (lambda ()
   ((compose
     (lambda (c) (+ c 1))
     (lambda (a b) (+ a b 1))) 1 2 3))
 "called with wrong nubmer of arguments")

