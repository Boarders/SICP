#lang scheme

;;;;;;; accumulation-function ;;;;;;;;;

(define (accum op neutral next start end)
  (define (accum-init total start end)
    (if (> start end)
        total
        (accum-init (op total start) (next start) end)))
  (accum-init neutral start end))


(define (accum-filter prop op neutral next start end)
  (define (accum-init total start end)
    (if (> start end)
        total
        (if (prop start)
            (accum-init (op total start) (next start) end)
            (accum-init total (next start) end))))

  (accum-init neutral start end))

;;;;;;; useful functions for testing ;;;;;;
(define (add x y)
  (+ x y))

(define (mult x y)
  (* x y))

(define (inc n)
  (+ n 1))

(define (triangular n)
  (accum add 0 inc 0 n))

(define (factorial n)
  (accum mult 1 inc 1 n))

(define (double-factorial n)
  (cond ((odd? n)  (accum-filter odd?  mult 1 inc 1 n))
        ((even? n) (accum-filter even? mult 1 inc 1 n))
  ))

(define (divides? d n)
  (= (remainder n d) 0))

(define (prime? n)
  (define (check-factors i)
    (if (> (* i i) n)
        #t
        (if (divides? i n)
            #f
            (check-factors (+ 1 i)))))
  (check-factors 2))

