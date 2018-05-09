#lang racket
(define (inc a)
  (+ a 1))

;1.2
(define (f n)
  (define (f-iter a b c k)
    (if (> k (- n 3)) c
        (f-iter b c (+ (* 2 b) (* 3 a) c) (+ k 1))
        ))
  (if (< n 3) n
      (f-iter 0 1 2 0)
      )
  )

;1.3 simple fibonacchi
(define (finb n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (if (even? count) (fib-iter a b p' q' (/ count 2))
      (fib-iter (+ (* b q) (* a q) (* a p)) (* (+ b p) (+ a q)) p q (- count 1))

      ))




