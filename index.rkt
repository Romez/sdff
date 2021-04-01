#lang racket
(require rackunit)

(define (identity x) x)

(define (square x) (* x x))

(define arity-table (make-hash))

(define (restrict-arity proc nargs)
  (hash-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-ref arity-table proc #f)
      (let ([a (procedure-arity proc)])
        a)))

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (iterate n)
  (lambda (f)
    (if (= n 0)
        identity
        (compose f ((iterate (- n 1)) f)))
    ))

;; (define ((iterate n) f)
;;   (if (= n 0)
;;       identity
;;       (compose f ((iterate (- n 1)) f))))

;; (((iterate 3) square) 5)


;; ((compose (lambda (x) (list "foo" x))
;;           (lambda (x) (list "bar" x)))
;;  'z)

;; (define (parallel-combine h f g)
;;   (define (the-combination . args)
;;     (h (apply f args) (apply g args)))
;;   the-combination)

;; ((lambda args (println args)) 'a 'b)

(define (parallel-combine h f g)
  (lambda args
    (h (apply f args) (apply g args))))

;; ((parallel-combine list
;;                    (lambda (x y z) (list 'foo x y z))
;;                    (lambda (u v w) (list 'bar u v w)))
;;  'a 'b 'c)

;; (define (spread-combine h f g)
;;   (let ([n (procedure-arity f)])
;;     (lambda args
;;       (h (apply f (take args n))
;;          (apply g (drop args n))))))

(define (spread-combine h f g)
  (let ([n (procedure-arity f)]
        [m (procedure-arity g)])
    (let ([t (+ n m)])
      (procedure-reduce-arity
       (lambda args
         (h (apply f (take args n))
            (apply g (drop args n)))) t))))

;; ((spread-combine list
;;                  (lambda (x y) (list 'foo x y))
;;                  (lambda (u v w) (list 'bar u v w)))
;;  'a 'b 'c 'd 'e)

(compose
 (lambda (c) (+ c 1))
 (lambda (a b) (+ a b 1)))
