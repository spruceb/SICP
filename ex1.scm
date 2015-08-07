;; 1

10
12
8
3
6
;; a
;; b
19
#f
4
16
6
16

;; 2

(/
 (+ 5 4
    (- 2 (- 3
	    (+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))

;; 3

(define (square n)
  (* n n))

(define (sum-sqr x y)
  (+ (square x) (square y)))

(define (f x y z)
  (sum-sqr
   (if (> x y)
      x
      y)
   (if (> z y)
       z
       y)))

;; 4

;; selects the "+" operator if b is greater than 0 and the "-"
;; operator otherwise, then applies that operator to (b a)
;; this will have the effect of adding a and |b|

;; 5

;; If it's applicative-order then there will be an inf-loop when p is
;; evaluated (because 0 and p will be evaluated before they're passed to test)
;; If it's normal-order then it will evaluate to 0, because test will expand
;; to (if (= 0 0) 0 (p)) and so p will never be evaluated

;; 6

;; It will loop forever, because new-if will evaluate all arguments before
;; itself evaluating, so sqrt-iter will continue to be evaluated with
;; more and more improved guesses forever before even the first one
;; is checked to be good enough

;; 7

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) (* guess 0.001)))
     

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt-it x)
  (sqrt-iter 1.0 x))


;; For very small numbers the test will fail because an early guess might be
;; concidered "good enough". For example, the sqrt of 0.000001. In this case
;; 0.01 could be concidered a good enough guess, because 0.01^2 == 0.0001, and
;; the difference between the 0.0001 and 0.000001 is only 0.000099, which is much
;; less than 0.001. And yet this is 10 times the actual square root.
;; 
;; For very large numbers subtracting the square of the guess from x will likely
;; yield an integer, because the square of the guess could be so large as to be truncated.
;; In this case 


;; 8

(define (cube-improve x y)
  (/ (+ (/ x (square y))
	(* 2 y))
     3))

(define (cube-enough? x y)
  (< (abs (- (cube-improve x y) y)) (* y 0.001)))

(define (cube-iter x y)
  (if (cube-enough? x y)
      y
      (cube-iter x (cube-improve x y))))

(define (cube-root x)
  (cube-iter x 1.0))


;; 9

;; first:
;; (+ 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (inc (+ (dec 3) 5)))
;; And so on, this is recursive
;; (+ (dec 4) (inc 5))
;; (+ (dec 3) (inc 6))
;; and so on, this is iterative

;; 10

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))
;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; ....

;; (f n) == (* 2 n)
;; (g n) == 2^n
;; (h n) == 2^(2^(n-1))

;; 11

(define (f-recurs n)
  (if (< n 3)
      n
      (+ (f-recurs (- n 1))
	 (* 2 (f-recurs (- n 2)))
	 (* 3 (f-recurs (- n 3))))))

(define (f-iter n)
  (define (f-iter-helper n count a b c)
    (if (> count n)
	c
	(f-iter-helper n (+ count 1)
		       b c
		       (+ (* 3 a) (* 2 b) c))))
  (if (< n 3)
      n
      (f-iter-helper n 3 0 1 2)))

;; 12

(define (pascal row column)
  (if (or (= column 1)
	  (= column row))
      1
      (+ (pascal (- row 1) (- column 1))
	 (pascal (- row 1) column))))

		 
;; 13

;; Note that \phi^1 / \sqrt{5} is (1 + \sqrt{5})/2*\sqrt{5}. Since \sqrt{5} is greater than one,
;; 1 + \sqrt{5} < 2*\sqrt{5}, and so phi^1/ \sqrt{5} < 1. However it is greater than 1/2, because
;; 1 + \sqrt{5} > \sqrt{5} = 1/2 * 2\sqrt{5}. So the closest integer to \phi^1/\sqrt{5} 1, which
;; is Fib(1).

;; Next, note that \phi^2 / \sqrt{5} is (1 + \sqrt{5})^2 / 4 \sqrt{5}, or
;; (1 + 5 + 2\sqrt{5}) / 4 \sqrt{5}, or (3 + \sqrt{5}) / 2\sqrt{5}. This is \approx 1.17...,
;; and so the closest integer is again 1, or Fib(2).

;; Now assume that int(\phi^k/\sqrt{5}) = Fib(k).
    
      
;; 14

;; 15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a. 5 times (12.15/3^5 is less than 0.1)
;; b. The number of steps are on the order of O(log(a)), because p will continue to be
;;    called until a is less than 0.1, and it will divide by 3 each time, so p should
;;    be called about one more time every time a is multiplied by three.
;;    The space required will be the same, as new variable bindings are required only
;;    for each new call of sine, p and cube, each of which are only required on a
;;    new call of p, which grows at O(log(a)).

;; 16

(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
	  ((even? n)
	   (expt-iter (* b b) (/ n 2) a))
	  (else
	   (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))

;; 17

(define (double n)
  (+ n n))
(define (halve n)
  (/ n 2))
(define (mult-r a b)
  (cond ((= b 0) 0)
	((even? b) (double (mult-r a (halve b))))
	(else (+ a (mult-r a (- b 1))))))

;; 18

(define (mult a b)
  (define (iter a b total)   
    (cond ((= b 0) total)
	  ((even? b)
	   (iter (double a) (halve b) total))
	  (else
	   (iter a (- b 1) (+ total a)))))
  (iter a b 0))

;; 19

;; Applying once gives us: (bq + aq + ap, bp + aq)
;; Applying twice gives: (q(bp + aq) + q(bq + aq + ap) + p(bq + aq + ap),
;;                        p(bp + aq) + q(bq + aq + ap))
;; = (bpq + aq^2 + bq^2 + aq^2 + aqp + bqp + aqp + ap^2,
;;    bp^2 + aqp + bq^2 + aq^2 + apq)
;; = (bp^2 + 2aqp + 2aq^2 + bq^2 + ap^2 + 2bqp,
;;    bp^2 + 2aqp + bq^2 + aq^2)
;; p' = (p^2 + q^2)
;; q' = (q^2 + 2qp)

(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b
		   (+ (square p) (square q))
		   (+ (square q) (* 2 q p))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
;; 20

;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (remainder a b))))

;; (gcd 206 40)

;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))
;; (if (= (remainder 206 40) 0) 1
;;     40
;;     (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (if (= 6 0)
;;     40
;;     (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (if (= (remainder 40 (remainder 206 40)) 0) 3
;;     (remainder 206 40)
;;     (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= (remainder 40 6) 0)
;;     (remainder 206 40)
;;     (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= 4 0)
;;     (remainder 206 40)
;;     (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) 7
;;     (remainder 40 (remainder 206 40))
;;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;; (if (= 2 0)
;;     (remainder 40 (remainder 206 40))
;;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) 14
;;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;     ...) ;....
;; (if (= 0 0)
;;     2
;;     ...)
;; 2
;;  18 for normal

;; (gcd 206 40)
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))
;;
;; One remainder calculated per iteration. 5 iterations, so 5 remainder.
(define (divides? a b)
  (= (remainder a b) 0))

(define (find-divisor n test)
  (cond ((> (square test) n) n)
	((divides? n test) test)
	(else (find-divisor n (+ test 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (and (> n 1)
       (= n (smallest-divisor n))))

(define (naive-exp-mod base power m)
  (remainder (expt base power) m))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	  (square (expmod base (/ exp 2) m))
	  m))
	(else
	 (remainder
	  (* base (expmod base (- exp 1) m))
	  m))))
;; 21

;; 199
;; 1999
;; 7

;; 22

(define (timed-prime-test n)
   (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (cond ((prime? n)
	 (report-prime (- (runtime) start-time))
	 #t)
	(else #f)))
(define (report-prime time)
  (display " *** ")
  (display time))


(define (search-for-primes start end count)
  (cond ((or (and (> start end) (>= end 0)) (<= count 0)) end)
	((timed-prime-test start)
	 (search-for-primes (+ 2 start) end (- count 1)))
	(else (search-for-primes
	       (+ start (if (even? start) 1 2))
	       end count))))

;; >1000
;; 1009
;; 1013
;; 1019
;; 0s

;; >10000
;; 10007
;; 10009
;; 10037
;; 0s

;; >100000
;; 100003 *** 9.999999999999981e-3
;; 100019 0s
;; 100043 0s

;; 1000000
;; ....
;; ....
;; 1000037 ** 1.0000000000000009e-2

;; ...

;; >100000000
;; 100000037 *** .01999999999999999

;; >1000000000
;; 1000000007 *** .04999999999999999

;; >10000000000
;; 10000000019 *** .15000000000000002

;; Smaller numbers are calculated too fast, but these larger numbers are increasing by about sqrt(10) each time, which is consistent with the expected values.

;; 23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divides? a b)
  (= (remainder a b) 0))

(define (find-divisor n test)
  (cond ((> (square test) n) n)
	((divides? n test) test)
	(else (find-divisor n (next test)))))

(define (smallest-divisor n)
  (find-divisor n 2))

;; Before:
;; 10000000019 *** .15000000000000002
;; Now:
;; 10000000019 *** .08000000000000007
;; Time was approx. halved, but actually t2 = 0.533... * t1
;; There is likely some  overhead for the calling of the function or setup or the builtins called by it that isn't halved by halving the steps

;; 24

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (timed-prime-test n)
  (start-prime-test n (real-time-clock)))
(define (report-prime time n)
  (newline)
  (display n)
  (display " *** ")
  (display time))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 15)
	 (report-prime (- (real-time-clock) start-time) n)
	 #t)
	(else #f)))

;; Very difficult to tell, as numbers on the order of 10^130 are still tested so fast (runtime) doesn't give useful results
;; Switched to real-time-clock:

;; (expt 10 10) -> 2
;; (expt 10 20) -> 3/4
;; (expt 10 30) -> 4/5
;; (expt 10 100) -> 18/20

;; This is approx. logarithmic (with some constant, obviously)

;; 25

;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))
;; if this is used instead of expmod then the times increase insanely. This seems to be because using exponentials on this scale involve simply massive numbers, large enough
;; that even the multiplications in fast-expt and the remainder procedure will take incredible amounts of time.

;; 26

;; Since by default scheme is applicative-order evaluation, all arguments are fully evaluated before being passed to a procedure.
;; So every time expmod has an even exponent, the same expmod is fully evaluated twice and then multiplied together. Since originally
;; the point of expmod was to make a linear process (repeated multiplication) logarithmic by halving the remaining steps at most points,
;; and this addition doubles the remaining steps and those same points, the total cancels out and stays linear.

;; 27

(define (fermat-prime? p a)
  (= (expmod a p p) a))
(define (all-fermat-prime? n)
  (define (iter n test)
    (if (= n test)
	#t
	(if (fermat-prime? n test)
	    (iter n (+ test 1))
	    #f)))
  (iter n 1))

(all-fermat-prime? 23) ; 23 is prime -> #t
(all-fermat-prime? 561) ; -> #t even though 561 = 3*11*17, and:
(prime? 561) ; -> #f
(all-fermat-prime? 1105) ; -> #t
;; So yes, the Carmichael numbers do satisfy this test even though they are not prime.

;; 28


(define (expmod-mr base power m)
  (define (mr-test ex m)
    (let ((r (remainder (square ex) m)))
      (if (or
	   (and (= r 1)
		(not (or (= ex 1) (= ex (- m 1)))))
	   (< ex 0))
	  -1
	  r)))
  (cond ((= power 0) 1)
        ((even? power)
         (mr-test (expmod base (/ power 2) m) m))
	(else
         (remainder
          (* base (expmod base (- power 1) m))
          m))))
(define (test-mr? p a)
   (= (expmod-mr a (- p 1) p) 1))
(define (mr-prime? p times)
  (define (random-test p)
    (test-mr? p ( + 1 (random (- p 1)))))
  (if (> times 0)
      (if (random-test p)
	  (mr-prime? p (- times 1))
	  #f)
      #t))

(define (mr-5-prime? p)
  (mr-prime? p 5))

;; --------------------------------------------------------------

;; 29 

(define (integral-simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k)
      (f (+ a (* k h))))
    (define (sum-iter i coeff n total)
      (if (= i n)
	  (+ total (y n))
	  (sum-iter (+ i 1)
		    (if (= coeff 4)
			2
			4)
		    n
		    (+ total (* coeff (y i))))))
    (* (/ h 3)  (sum-iter 1 4 n (y 0)))))

;; Much more accurate, without decimal input even seems to get exact values often

;; 30

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

;; 31

;; a

(define (product-recurse f a next b)
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))
(define (product-iter f a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (f a)))))
  (iter a 1))
(define (identity x) x)
(define (inc n) (+ 1 n))
(define (dec n) (- n 1))

(define (factorial n)
  (product-iter identity 1 inc n))

(define (pi-approx n)
  (define (+-2 x) (+ x 2))
  (* 8
     (/ (product-iter square 4 +-2 (- n 1))
	(product-iter square 3 +-2 n))))
  
		

;; 32

;;  a

(define (accumulate-recurse combine initial term a next b)
  (if (> a b)
      initial
      (combine (term a)
	       (accumulate-recurse combine initial term (next a) next b))))
;; b

(define (accumulate-iter combine initial term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combine (term a) result))))
  (iter a initial))
(define accumulate accumulate-iter)
(define (sum-accum f a next b)
  (accumulate-iter + 0 f a next b))
(define (product-accum f a next b)
  (accumulate-iter * 1 f a next b))

;; 33

(define (filtered-accumulate predicate? combine initial term a next b)
  (if (predicate? a)
      (if (> a b)
	  initial
	  (combine (term a)
		   (filtered-accumulate predicate? combine initial term (next a) next b)))
      (filtered-accumulate predicate? combine initial term (next a) next b)))

(define (prime-sum-square a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (relatively-prime? a b)
  (= (gcd a b) 1))

(define (b-fn n)
  (define (f? a)
    (relatively-prime? a n)) 
  (filtered-accumulate f? * 1 identity 1 inc n))

;; 34

;; You'll get an error saying that 2 isn't a function. (f f) will turn into (f 2), which in
;; turn is (2 2), which is an error.

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (fixed-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1))

;; 35

;; 1 + 1/((1 + sqrt(5))/2) = 1 + 2/(1 + sqrt(5) = (1 + sqrt(5))/(1+sqrt(5)) + 2/(1+sqrt(5))
;; = (3 + sqrt(5))/(1+sqrt(5)) = (3 + sqrt(5) -3sqrt(5) -5)/(1 - 5) = (-2 - 2sqrt(5))/(-4)
;; = (-1 - sqrt(5))/(-2)  = (1 + sqrt(5))/2

(define fixed-golden-ratio
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.))

;; 36

(define (printing-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; Finding fixed points of log(1000)/log(x) without average damping gives about a page of output
;; With gives about 8 tries.

;; 37

(define (cont-frac n d k)
  (define (recurse i)
    (if (= i k)
	(/ (n k) (d k))
	(+ (d (- i 1)) (/ ( n i) (recurse (+ i 1))))))
  (/ (n 1) (recurse 2)))

;; 13 iterations is sufficant for 4 decimal accuracy

(define (cont-frac-iter n d k) 		;recurse
  (define (iter i result)
    (if (= i 0)
	result
	(iter (- i 1)
	      (+ (d i) (/ (n (+ i 1)) result)))))
  (/ (n 1) (iter (- k 1) (d k))))

;; 38

(define (e-cf k)
  (define (d i)
    (if (= 0 (remainder (+ i 1) 3))
	(* (/ 2 3) (+ i 1))
	1))
  (+ 2 (cont-frac (lambda (x) 1.) d k)))

;; 39

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1)
				  x
				  (- (square x))))
		  (lambda (i) (+ 1 (* 2 (- i 1))))
		  k))

;; ------------------------------

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (derive g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.000001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((derive g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newtons-method
   (lambda (y) (- (square y) x )) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (trasnform g) guess))

(define (sqrt-a x)
  (fixed-point-of-transform
   (lambda y (/ x y)) average-damp 1.0))

(define (sqrt-a x)
  (lambda (y) (- (square y) x)) newton-transform 1.0)

;; 40

(define (cubic a b c)
  (lambda (y) (+ (cube y) (* a (square y)) (* b y) c)))
(define (cubic-zero a b c)
  (newtons-method (cubic a b c) 1))

;; 41

(define (double f)
  (lambda (x) (f (f x))))

;; 21. The first call (double double) returns a function that takes a function and applies
;; it to the result of calling it on its argument 4 times.

;; 42

(define (compose f g)
  (lambda (x) (f (g x))))

;; 43

(define (repeated f n)
  (define (recurse i)
    (if (= i 1) f
	(compose f (recurse (- i 1)))))
  (recurse n))

;; 44
(define (average-3 a b c)
  (/ (+ a b c) 3))
(define (smooth-dx f dx)
  (lambda (x) (average-3
	       (f (- x dx))
	       (f x)
	       (f (+ x dx)))))

(define (smooth f)
  (smooth-dx f dx))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

;; 45

;; Testing shows that roots require an extra damping at each power of 2
(define (log2 n)
  (/ (log n) (log 2)))
(define (nth-root n x)
  (fixed-point
   ((repeated average-damp
	      (floor (log2 n)))
    (lambda (y) (/ x (expt y (- n 1))))) 1))
    
;; 46

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
	guess
	(iter (improve guess))))
  iter)

(define (sqrt-imp x)
  ((iterative-improve
   (lambda (guess) (< (abs (- x (square guess))) tolerance))
   (lambda (guess) (average guess (/ x guess))))
   x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (fixed-point f first-guess)   
  ((iterative-improve
   (lambda (guess) (< (abs (- guess (f guess))) tolerance))
   f) first-guess))

