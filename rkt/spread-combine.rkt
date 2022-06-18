#lang racket

(require 
 (prefix-in ar: "./arity.rkt"))

(define (spread-combine h f g)
  (let* ([n (ar:get-arity f)]
         [m (ar:get-arity g)]
         [l (ar:get-arity h)]
         [combinator-arity (cond
                             [(or (arity-at-least? n)
                                  (arity-at-least? m))
                              (arity-at-least
                               (if (number? n)
                                   (+ n (arity-at-least-value m))
                                   (+ m (arity-at-least-value n))))]
                             [else
                              (+ n m)])])
     
    (when (and (arity-at-least? n)
               (arity-at-least? m))
      (error "At least one procedure must have fixed arity"))
    
    (when (not (= l 2))
      (error "Hander arity missmatch"))
        
    (define (the-combination . args)
      (let ([divide-pos (if (number? n)
                            n
                            (- (length args) m))])
        
        (when (not (arity-includes? combinator-arity (length args)))
          (error "Arity missmatch"))
        
        (h (apply f (take args divide-pos))
           (apply g (drop args divide-pos)))))

    ;; fix for at least arity
    (ar:restrict-arity
     the-combination
     combinator-arity)))

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

[module+ test
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
                "advertises correct arity")

  (check-exn exn:fail?
             (lambda ()
               (spread-combine
                (lambda (x y) (+ x y))
                +
                +))
             "both procedures can't have multiple arities")

  (check-equal? ((spread-combine
                  (lambda (x y) (+ x y))
                  (lambda (b) b)
                  +
                  ) 1 1 1)
                3
                "use second procedure with at-least arity")

  (check-equal? ((spread-combine
                  (lambda (x y) (+ x y))
                  +
                  (lambda (b) b)
                  ) 1 1 1)
                3
                "use first procedure with at-least arity")

  (check-exn exn:fail?
             (lambda ()
               ((spread-combine
                 (lambda (x y) (+ x y))
                 (lambda (a) a)
                 (lambda (b) b))
                1 1 1))
             "break combinator arity for fixed arity")
  ]
