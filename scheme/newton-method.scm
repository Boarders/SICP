#lang scheme

; Helper functions
(define (reduce op z lst)   
  (cond ((null? lst) z)
    (else (reduce op 
                  (op z (car lst))
                  (cdr lst)))))

;;;;;;;;;;; Polynomials  and Newton's method;;;;;;;;;;

;; a polynomial is represented as an ordered list of pairs
;; where a pair `(a . n) corresponds to a term ax^n.

;; to find the derivative we simply do the term-wise derivative
(define (polynomial-derivative poly)
  (map term-derivative poly)
)

;; The termwise derivative takes the derivative of a term of
;; the form a x^n giving back na x^(n - 1). We need to take
;; care when the term is a constant.
(define (term-derivative term)
    (let ((coeff (car term))
          (power (cdr term)))
    (cond ((= power 0) '(0 . 0))
          (else (cons (* power coeff) (- power 1)))
    ))
  )

;; This function evaluates a polynomial at a value.
(define (polynomial-eval poly val)
  (let ((fold-fun (lambda (acc x) (+ (term-eval x val) acc))))
  (reduce fold-fun 0 poly)))

;; This function evaluates a polynomial term at a value.
(define (term-eval term val)
  (let   ((coeff (car term))
          (power (cdr term)))
    (* coeff (expt val power))))

;
(define (newton-iterate poly guess)
  (- guess
     (/
      (polynomial-eval poly guess)
      (polynomial-eval (polynomial-derivative poly) guess)
     )
   )
)

;; Newton's method where we explicitly pass the number of iterations
(define (newton-method-iter iter poly guess)
  (let ((new-guess (newton-iterate poly guess)))
  (if (= iter 0) guess
      (newton-method-iter (- iter 1) poly new-guess)
   )))
      
      
;; Example polynomial x^2 - 2 which can be used to find
;; the square root of two
(define ex (list '(1 . 2) '(-2 . 0)))
  