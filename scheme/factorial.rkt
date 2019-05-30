#lang scheme

;;;;;;;;;;;;;;; Factorial functions ;;;;;;;;;;;;;;;

;; recursive factorial without tail recursion
(define (slow-factorial n)
  (if (= n 1)
      1
      (* n (slow-factorial (- n 1)))
      )
  )


;; factorial function with explicit tail recursion
(define (tail-factorial n)
  (define (tail-iter acc ind)
    (if (= ind 1)
        acc
        (tail-iter (* ind acc) (- ind 1))
    ))
  (tail-iter 1 n))


;; factorial using list functions defined below
(define (list-factorial n)
  (product- (enumFromTo 1 n)))

;;;;;;;; General Definitions ;;;;;;;;;;;;;;;

;; Forms the list from i to j
(define (enumFromTo i j)
  (define (enumFromTo-iter i j acc)
    (if (> i j)
        acc
        (enumFromTo-iter i (- j 1) (cons j acc))
    )
  )
  (enumFromTo-iter i j (list)))

;; Takes the product of a list
(define (product ns)
  (define (product-iter ns acc)
    (if (null? ns)
        acc
        (product-iter (cdr ns) (* (car ns) acc))
    ))
  (product-iter ns 1))

;; Same as product but with a general monoidal operator
(define (foldMap op neutral ns)
  (define (foldMap-iter ns acc)
    (if (null? ns)
        acc
        (foldMap-iter (cdr ns) (op (car ns) acc))
    ))
  (foldMap-iter ns neutral))

(define (product- ns)
  (foldMap * 1 ns))