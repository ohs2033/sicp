#lang racket
;Chapter1.3

; Implements sigma

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)
         )))

; let's make integral

(define (integral f a b dx)
  (define (add-dx k)
    (+ k dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx)
  )

(define (cube x)
  (* x x x)
  )
(define (inc x)
  (+ 1 x)
  )

;1.29 simpson's rule

(define (simpson f a b n)
   (define h (/ (- b a) n))
   (define (yk a k) (f (+ a (* k h))))
   (define (each k)
     (cond ((or (= k 0) (= k n)) (yk a k))
           ((odd? k) (* 4.0 (yk a k)))
           ((even? k)(* 2.0 (yk a k)))
           ))
  (* (/ h 3.0) (sum each 0 inc n))
  )

;1.30 implement sum liner iterative way.

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a) ) )))
    (iter a 0))



;1.31 product

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))
      )
  )

(define (piterm k)
  (if (even? k)
      (/ (* 2.0 (+ k 1.0)) (+ 1.0 (* 2.0 (+ k 1.0))))
      (/ (* 2.0 (+ k 1.0)) (+ 1.0 (* 2.0 k)))
))

;1.31&1.32


(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))
))


(define (double x)
  (* 2 x))

; 반복으로 짠 accumulate
(define (accumulate2 combiner null-value term a next b)
  (define (ac-iter result a b)
     (if (> a b)
         result
         (ac-iter (combiner result (term a)) (next a) b)))
   (ac-iter null-value a b))


; fixed point
(define (abs a b)
  (if (> a b)
      (- a b)
      (- b a)))


; uclid a

 (define (fixed-point f init)
   (let (
         (epsilon 0.001)
         )
     (if (< (abs (f init) init) epsilon)
         init
         (fixed-point f (f init)))
     ))
         


;1.3.3


(define (func-avg f)
  (lambda (x) (/ (+ (f x) x ) 2))
)


(define (deriv g)
  (let* ((dx 0.0000001))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx) )
    ))
  