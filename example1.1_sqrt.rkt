#lang sicp

(define guess 1)
(define x 2)
(define (square a) (* a a))
(define (abs a)
  (if (> a 0) a
      (- a)
  )
)

(define (good-enough? guess x)
  (> 0.0000000000000000000000000000000000000000001
                       (abs (- x (square guess) ))))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
     guess
     (sqrt-iter (improve guess x)
                x)))


(define (improve guess x)
(average guess (/ x guess)))

(define (average x guess)
(/ (+ x guess) 2))

(define (good? x)
  (< x 0.0001))



(define (sqrt n startval)
  (let (temp (/ n startval))
    (if 