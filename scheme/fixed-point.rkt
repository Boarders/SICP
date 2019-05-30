#lang scheme

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x) 
        x
        ((iterative-improve good-enough? improve) (improve x))
    )
  )
)

(define (sqrt x)
  (define (square x) (* x x))
  (define (average x y)
          (/ (+ x y) 2))
  (define tol 0.001)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tol))
  ((iterative-improve good-enough? improve) 1.0)
)
