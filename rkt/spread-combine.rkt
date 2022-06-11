#lang racket

(require 
 (prefix-in ar: "./arity.rkt"))

(define (spread-combine h f g)
  (let ([n (ar:get-arity f)]
        [m (ar:get-arity g)]
        [l (ar:get-arity h)])

    (when (not (= l 2))
      (error "Arity missmatch"))
    
    (define (the-combination . args)
      (when (not (= (length args) (+ n m)))
        (error "Arity missmatch"))
      
      (h (apply f (take args n))
         (apply g (drop args n))))
    
    (ar:restrict-arity
     the-combination
     (+ n m))))

;; (define (spread-apply f g)
;;   (let ((n (ar:get-arity f))
;;         (m (ar:get-arity g)))
;;     (let ((t (+ n m)))
;;       (define (the-combination . args)
;;         (when (not (= (length args) t))
;;           (error "ERROR")
;;           )
;;         (values (apply f (list-head args n))
;;                 (apply g (list-tail args n))))
;;       (restrict-arity the-combination t))))

#;
(let ([n (procedure-arity f)]
      [m (procedure-arity g)])
  (when (not (number? n))
    (error "first proc shoud have fixed arity"))
  (let ([t (cond
             [(arity-at-least? m) (arity-at-least (+ n (arity-at-least-value m)))]
             [(number? m) (+ n m)]
             [(cons? m) (list (+ n (car m))
                              (+ n (cadr m)))])])
    (procedure-reduce-arity
     (lambda args
       (when (not (= (length args)))
         (error "wrong arity"))
       (h (apply f (take args n))
          (apply g (drop args n))))
     t)))

(module+ test
  (require rackunit)

  (check-equal?
   ((spread-combine (lambda (x y) (- x y))
                    (lambda (a b) (+ a b))
                    (lambda (c) c))
    2 2 4)
   0)

  (check-equal?
   ((spread-combine (lambda (x y) (+ x y))
                    (lambda (x) x)
                    (spread-combine
                     (lambda (x y) (+ x y))
                     (lambda (x) x)
                     (lambda (x) x)))
    1 1 1)
   3
   "spread-combine in spread-combine")
  
  (check-exn exn:fail?
             (lambda ()
               (spread-combine
                (lambda (x y z) (+ x y z))
                (lambda (a) a)
                (lambda (b) b)))
             "incompatable arity")

  (check-exn exn:fail?
             (lambda ()
               ((spread-combine
                  (lambda (x y z) (+ x y z))
                  (lambda (a) a)
                  (lambda (b) b)) 'x 'y 'z))
             "called with incompatable arity")

  (check-equal? (ar:get-arity (spread-combine
                               (lambda (x y) (+ x y))
                               (lambda (a) a)
                               (lambda (b) b)))
                2
                "advertises correct arity"))

