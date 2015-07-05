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

(define (sqrt x)
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
  (= n (smallest-divisor n)))

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
