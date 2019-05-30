#lang scheme

(define (compose f g)
  (lambda (x) (f (g x))))

(define (add1 x) (+ 1 x))
(define (times2 x) (* 2 x))
(define (id) (lambda (x) x))

(define (repeated n f)
  (if (= 0 n)
      (id)
      (compose f (repeated (- n 1) f))
  )
)

(define (smooth f)
  (define (average x1 x2 x3)
          (/ (+ x1 x2 x3) 3))
  (define dx 0.0001)
  (lambda (x) (average 
                       (f (- x dx))
                       (f x)
                       (f (+ x dx))
              )
          )
)

(define (n-smooth n f)
  ((repeated n smooth) f))
