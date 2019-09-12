#lang scheme

(define (succ n) (+ n 1))

;; church encoded zero
(define c-zero
  (lambda (f) (lambda (x) x)))

;; church encoded add-1
(define (add-1 cn)
  (lambda (f) (lambda (x) (f ((cn f) x))))
  )

                      

;; addition of chruch numerals
(define (c-add cn cm)
  (lambda (f) (lambda (x) ((cn f) ((cm f) x))))
  )

;; mulitplication of church numerals
(define (c-mult cn cm)
  (lambda (f) (lambda (x) ((cn (lambda (x) ((cm f) x))) x)))
  )

;; exponential of chrurch numerals
(define (c-exp cn cm)
  (lambda (f) (lambda (x) (((cm cn) f) x))))
                      

;; convert from church numeral to literal
(define (to-Nat cn)
  ((cn succ) 0))


;; example values
(define c-one
  (lambda (f) (lambda (x) (f x))))

(define c-two
  (add-1 c-one))

(define c-three
  (add-1 c-two))

(define c-four
  (c-add c-two c-two))

(define c-six
  (c-mult c-two c-three))

(define c-nine
  (c-exp c-three c-two))

(define c-sixty-four
  (c-exp c-four c-three))



