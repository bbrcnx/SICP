#lang racket

;;1.2.5 Greatest Common Divisors

;;Searching for divisors
(define (square a)
  (* a a))
(define (divide? a n)
  (= (remainder n a) 0))

(define (smallest-divisor n)
  (find-divisor 2 n))

(define (find-divisor a n)
  (cond
    ((> (square a) n) n)
    ((divide? a n) a)
    (else (find-divisor (+ a 1) n))))

(define (prime? n)
  (= n (smallest-divisor n)))


;;Fermat test
;;A与B相乘后再算与C相除的remainder，和A除C的remainder乘以B除C的remainder相等。
;;前一种做法会导致特别大的数算起来会很慢，而后一种做法可以避免掉大数的出现。
(define (expmod a exp n)
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder
      (square (expmod a (/ exp 2) n))
      n))
    (else
     (remainder
      (* a (expmod a (- exp 1) n))
      n))))

(define (test-prim n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random(- n 1)))))

(define (fast-prim n times)
  (cond
    ((= times 0) true)
    ((test-prim n) (fast-prim n (- times 1)))
    (else false)))
  

;;exercise 1.21
;;(smallest-divisor 199)
;;=199
;;(smallest-divisor 1999)
;;=1999
;;(smallest-divisor 19999)
;;=7

;;exercise 1.22
(define (timed-prim-test n)
  (newline)
  (display n)
  (start-time-test n (runtime)))

(define (start-time-test n start-time)
  (cond ((prime? n)
      (report-prime (- (runtime) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (runtime) (current-inexact-milliseconds))

(define (multi-prim start-number counts)
  (timed-prim-test start-number)
  (cond
    ((> counts 0) (multi-prim (+ start-number 2) (- counts 1)))))

;;excersice 1.23
(define (square a)
  (* a a))
(define (divide? a n)
  (= (remainder n a) 0))

(define (smallest-divisor n)
  (find-divisor 2 n))

(define (find-divisor a n)
  (cond
    ((> (square a) n) n)
    ((divide? a n) a)
    (else (find-divisor (next a) n))))

(define (next a)
  (if (= a 2) 3
      (+ a 2)))
        

(define (prime? n)
  (= n (smallest-divisor n)))

;;exercise 1.24
(define (timed-prim-test n)
  (newline)
  (display n)
  (start-time-test n (runtime)))

(define (start-time-test n start-time)
  (cond ((prime? n)
      (report-prime (- (runtime) start-time)))))

(define (prime? n)
  (fast-prime n 100))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (runtime) (current-inexact-milliseconds))

(define (multi-prim start-number counts)
  (timed-prim-test start-number)
  (cond
    ((> counts 0) (multi-prim (+ start-number 2) (- counts 1)))))

;;exercise 1.25
;;It is correct however it will produce a huge number that will take
;;much longer time to deal with.

;;exercise 1.26
;;Because this method have two expmod. the normal way just calculate one and
;;a multiply on number.

;;excersice 1.27
(define (square n )
  (* n n))
(define (expmod a exp n)
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder
      (square (expmod a (/ exp 2) n))
      n))
    (else
     (remainder
      (* a (expmod a (- exp 1) n))
      n))))

(define (iter2 a n)
  (cond ((not (= (expmod a n n) a)) (display a)))
  (cond ((< a (- n 1)) (iter2 (+ a 1) n))))


(define (sum a b)
  (if (> a b)
      0
      (+ a (sum (+ a 1) b))))
;;(define (<name>  a  b)
;;  (if (> a b)
;;      0
;;      (<term> a (<name> <next> a b))))

(define (sum2 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube a)
  (* a a a))
(define (inc n)
  (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identiy x)
  x)
(define (sum-integer a b)
  (sum identiy a inc b))
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-sum a pi-next b))

  
;;exercise 1.30
(define (sum3 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;exercise 1.31
;;a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial a b)
  (define (identity x) x)
  (define (inc x)
    (+ x 1))
  (product identity a inc b))

(define (pi a b)
  (define (pi-term x)
    (/ (* x (+ x 2))
       (square (+ x 1))))
  (define (pi-next x)
    (+ x 2))
  (define (square x)
    (* x x ))
  (* 4 (product pi-term a pi-next b)))

;;b
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1.0))


(define (pi-iter a b)
  (define (pi-term x)
    (/ (* x (+ x 2))
       (square (+ x 1))))
  (define (pi-next x)
    (+ x 2))
  (define (square x)
    (* x x ))
  (display (* 4 (product-iter pi-term a pi-next b))))

(define (runtime) (current-milliseconds))
(define (checktime a b t)
  (pi-iter a b)
  (newline)
  (- (runtime) t))


;;exercise 1.32

;;a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;;(define (add) +)
;;(define (zero x) x)
;;(define (term x) x)
;;(define (next x) (+ x 1))

;;b
(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate2 combiner (combiner (term a) null-value) term (next a) next b)))



