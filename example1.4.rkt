#lang racket

;problem 2.2

;start-segment, end-segment, make-segment

(define (avg a b)
  (/ (+ a b) 2))

(define (make-point x y)
  (cons x y))

(define (make-line x y)
  (cons x y))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment a b)
  (cons (avg (car a) (car b)) (avg (cdr a) (cdr b))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))



; problem 2.3


; problem 2.5 ( from class ) 
(define (sq a) (* a a))
(define (pow a n)
  (if (= n 0) 1
  (if (even? n)
      (sq (pow a (/ n 2)))
      (* a (sq (pow a (/ (- n 1) 2)))))))

(define (pow2 b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((odd? n) (iter (* a b) b (- n 1)))
          (else (iter
                 a
                 (sq b)
                 (/ n 2)))))
  (iter 1 b n))




         
(define (get-degree base input)
  (define (iter count input)
    (if (= (modulo input base))
        (iter (* count 1) (/ input base))
        count))
  (iter 0 input))

(define (car1 n)
  (get-degree 2 n))

(define (cdr1 n)
  (get-degree 3 n))
    
                    


;2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y)))
  )

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))



(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
  (error "devided by 0 is not possible")
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

(define (make-interval a b)(cons a b))

(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (ub x)(upper-bound x))
(define (lb x)(lower-bound x))

(define (width x)(/ (- (upper-bound x) (lower-bound x)) 2))


;2.11

;(define (effective-mul-iv x y)
 ;d (if (< (lb x) 0)))
                         
                    
;2.12

      (define (make-center-interval c e)
        (let ((p (/ e 100)))
          (cons (+ c (* c p)) (- c (* c p)))))

;2.13

