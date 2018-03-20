#lang racket

;;1.2.1 linear recursion and iteration.
(define (a x y)
	(cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (a (- x 1) (a x (- y 1))))))


(define (+ a b)
	(if (= a 0) 
		b
		(inc (+ (dec a) b))))
(define (+ a b)
	(if (= a 0) 
		b
		(+ (dec a) (inc b))))
	

(define (factorial x)
	(define (fact-cunt counter product)
		(if (> counter x)
			product
			(fact-cunt (+ counter 1) (* counter product ))))
	(fact-cunt 1 1))


(define (factorial x)
	(if (= x 1)
		1
		(* x (factorial (- x 1)))))
 

(define (sqrt x)
	(define (abs a)
		(if (< a 0)
			(- a)
			a))
	(define (good guess)
		(< abs(- (* guess guess) x) 0.00001)))
	(define (improve guess)
		(/ (+ guess (/ x guess)) 2))
	(define (sqrt-iter guess)
		(If (good guess)
			guess
			(sqrt-iter (improve guess)))
	(sqrt-iter 1.0))

;;1.2.2 Tree recursion

(define (fib n)
	(cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))))
		
(define (fib n)
	(define (fib-iter a b count)
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))))
	(fib-iter 1 0 n))

(define (change amount)
	(cc amount 5))
	
(define (cc amount kinds)
	(cond 
		((= amount 0) 1)
		((or (< amount 0) (= kinds 0)) 0)
		(else (+ (cc amount (- kinds 1))
			(cc (- amount (denom kinds)) kinds)))))	
(define (denom kinds)
	(cond
		((= kinds 1) 1)
		((= kinds 2) 5)
		((= kinds 3) 10)
		((= kinds 4) 25)
		((= kinds 5) 50)))

;;exercise 1.11
(define (f n)
	(if (< n 3) 
		n
		(+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))
(f 5)
(+ (f 4) (f 3) (f 2))

;;exercise 1.12
(define (pascal a b)
	(cond
		((= a 1) 1)
		((= b a) 1)
		(else (+ (pascal (- a 1) (- b 1)) (pascal (- a 1) (+ b 1))))))

;;1.2.4 Exponentiation
(define (exp b n)
	(if (= n 0)
		1
		(* b (exp b (- n 1)))))
		
(define (exp b n)
	(define (exp-iter b n product)
		(if (= n 0)
			product
			(exp-iter b (- n 1) (* b product))))
	(exp-iter 2 3 1))

(define (exp b n)
	(cond ((= n 0) 1)
		((even? n)
             (square (exp b ( / n 2))))
		(else 
            (* b  (exp b  (- n 1))))))
           
;;exercise 1.16
(define (exp b n)
	(define (exp-iter product b n)
		(cond 
			((= n 0) 1)
			((even? n) (exp-iter product (square b) (/ n 2)))
			(else (exp-iter (* product b) b (- n 1)))))
	(define 1 b n))

 
;;exercise 1.17
(define (* a b)
	(if (= b 0)
		0
		(+ a (* a (- b 1)))))

(define (* a b)
	(cond
		((= b 0) 0)
		((even? b) (+ (double a) (* a (halve b))))
		(else (+ a (* a (- b 1))))))

;;exercise 1.18
(define (* a b)
	(define (iter c a b)
		(cond
			((= b 0) c)
			((even? b) (iter c (double a) (halve b)))
			(else (iter (+ c a) a (- b 1)))))
	(iter 0 a b))


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

;;1.3.1 Procedure as arguments

(define (sum a b)
	(if (> a b)
		0
		(+ a (sum (+ a 1) b))))
		
(define (sum-cubes a b)
	(if (> a b)
		0
		(+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2)))
		   (pi-sum (+ a 4) b))))

(define (<name> term a next b
	(if (> a b)
		0
		(+ (<term> a) (<name> (<next a) b )))))		   


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


;;1.3.2 Constructing Procedures Using Lambda

(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x ( + x 2))))
       a
       (lambda (x) (+ x 1))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))


;;The following two procedures are same.
(define (plus4 x) (+ x 4))
(define plus42 (lambda (x) (+ x 4)))

((lambda (x y z) (+ x y (square z)))
 1 2 3)

;;Using let to create local variables

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f2 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f3 x y a b)
  (+ (* x (square a))
     (* y b)
     (* a b)))

(f3 x y (+ 1 (* x y)) (- 1 y))

(define (f4 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;Let is local. For example if x is 5. the value will be 38
(+ (let ((x 3))
     (+ x (* x 10)))
   x)

;;If x is 2, the value will be 12.
(let ((x 3)
      (y (+ x 2)))
  (* x y))


;;exercise 1.34
;;(f f)
;;(f 2)
;;(2 2)

;;1.3.3 Procedures as General Methods

;;Finding roots of equations by the half-iterval method

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
       (if (close-enough? neg-point pos-point)
           mid-point
           (let ((test-value (f mid-point)))
             (cond ((positive? test-value)
                    (search f neg-point mid-point))
                   ((negative? test-value)
                    (search f mid-point pos-point))
                   (else mid-point))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average a b) (/ (+ a b) 2.0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "value are not opposite sign" a b)))))


(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;exercise 1.35
;;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0)

;;exercise 1.36
(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;;2.1.1 Example: Arithmatic operations for rational numbers.
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;pairs
(define x (cons 1 2))
;;cons stands for "construct"

;;(car x)
;;car stands for contents of address part of register.
;;(cdr x)
;;cdr stans for contents of decrement part of register.

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

;;(car (car z))
;;1
;;(car (cdr z))
;;3

;;The following way is more efficient.
;;(define make-rat cons)
;;(define numer car)
;;(define denom cdr)

(define (make-rat x y) (cons x y))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(define (make-rat2 n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
        

;;exercise 2.1
;;solution 1
(define (make-rat3 x y)
  (let ((g (gcd x y)))
    (cond ((and (< x 0) (< y 0))
           (cons (* -1 (/ x g)) (* -1 (/ y g))))
          ((< y 0)
           (cons (/ x g) (/ y g)))
          (else (cons (/ x g) (/ y g))))))
;;solution 2
(define (make-rat4 n d)
  (let ((g ((if (< d ) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))