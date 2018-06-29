#lang sicp
(#%require sicp-pict)
(define average (lambda (v1 v2) (/ (+ v1 v2) 2)))

(define square (lambda (n) (* n n)))

(define (sqrt n)
  (define (improve guess)
     (average guess (/ n guess)))
  (define (good-enough? guess)
     (< (abs (- (square guess) n)) 0.0000000001))
  (define (tryit guess)
     (if (good-enough? guess)
        guess
        (tryit (improve guess))))
  (tryit 1))

; Let's define cons and car, cdr.
(define (cons-fake x y)
  (lambda (x) (if (= x 1) x
                  y)
    ))

(define (car-fake a)
  (a 1))

(define (cdr-fake a)
  (a 2))

; Answer

(define (cons-f2 x y)
  (lambda (m) (m x y)))

(define (car-f2 p)
  (p (lambda (x y) x)))

(define (cdr-f2 p)
  (p (lambda (x y) y))
  )

;2.1 review

;pair

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  )

(define one-half (make-rat 1 2))

(print-rat one-half)

;2.2

(define (make-segment x y)
  (cons x y))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (midpoint-segment seg)
  (cons
   (/ (+ (x-point  (start-segment seg))
         (x-point (end-segment seg))) 2)
   
   (/ (+
       (y-point (start-segment seg))
       (y-point (end-segment seg))) 2)))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(print-point (midpoint-segment
              (make-segment (make-point 1 4) (make-point 3 10))))


;2.3



(define (length seg)
  (sqrt (+
         (square
           (-
            (x-point (start-segment seg))
            (x-point (end-segment seg))))
         (square
          (-
           (y-point (start-segment seg))
           (y-point (end-segment seg)))))))

(define (rec line1 line2)
  (cons line1 line2))



(define (get-circum rec)
  (* 2 (+ (length (cons rec)) (length (cdr rec)))))


(define (get-area rec)
  (* (length (cons rec)) (length (cdr rec))))



;2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;2.1.4


(define (make-interval low up)
  (cons low up)
  )

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (get-max x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (lower-bound x) (upper-bound y))))
    (max p1 p2 p3 p4)))

(define (get-min x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (lower-bound x) (upper-bound y))))
    (min p1 p2 p3 p4)))
         
(define (mul-interval x y)
  (make-interval
   (get-min x y)
   (get-max x y)))

(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
          (= (lower-bound y) 0))
      (error "second interval must not contain zero.")
  (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y))))))

;2.8 

(define (sub-interval x y)
  (make-interval
   (- (lower-bound x)
      (upper-bound y))
   (+ (upper-bound x)
      (upper-bound y))))

;2.9
(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

(define int1 (make-interval 6.8 8.9))
(define int2 (make-interval 4.1 12.3))
(newline)

(define mulval (mul-interval int1 int2))
(newline)
(define divval (div-interval int1 int2))
(newline)

(define subval (sub-interval int1 int2))

;2.10
;done

(define (center interval)
  (/ (+ (upper-bound interval)
        (lower-bound interval))
     2))

;2.11
(define (per number)
  (/ number 100))
(define (make-center-percent middle error)
  (cons
   (- middle
      (* (per error)
         middle))
   (+ middle
      (* (per error)
         middle))))


(define (percent int)
  (let (
        (intval (center int))
        (up (upper-bound int))
        )
    (* (/ (- up intval) intval) 100)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define ex1 (make-center-percent 4 10))
(define ex2 (make-center-percent 5 10))



