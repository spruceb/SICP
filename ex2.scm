;; chapter 2
(load "ex1.scm")
;; 1
(define (positive? n) (< 0 n))
(define (negative? n) (> 0 n))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let* ((g (gcd n  d))
	 (n (/ n g))
	 (d (/ d g)))
    (if (negative? d)
	(cons (- n) (- d))
	(cons n d))))
(define (numer rat) (car rat))
(define (denom rat) (cdr rat))
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
(define (print-rat x) (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 2

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))


(define (print-point p) (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint segment)
  (make-point
   (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
   (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

;; 3

(define (make-rectangle starting-point height width)
  (cons starting-point (cons height width)))
(define (height rectangle)
  (car (cdr rectangle)))
(define (width rectangle)
  (cdr (cdr rectangle)))
(define (upper-left-corner rectangle)
  (car rectangle))
(define (upper-right-corner rectangle)
  (make-point (+ (width rectangle)
		 (x-point (upper-left-corner rectangle)))
	      (y-point (upper-left-corner rectangle))))
(define (lower-left-corner rectangle)
  (make-point (x-point (upper-left-corner rectangle))
	      (+ (height rectangle)
		 (y-point (upper-left-corner rectangle)))))
(define (lower-right-corner rectangle)
  (make-point (+ (width rectangle)
		 (x-point (upper-left-corner rectangle)))
	      (+ (height rectangle)
		 (y-point (upper-left-corner rectangle)))))

(define (perimeter rect)
  (+ (* 2 (width rect))
     (* 2 (height rect))))
(define (area rect)
  (* (width rect) (height rect)))


(define (make-rectangle top-side left-side)
  (cons top-side left-side))
(define (height rect)
  (let ((start (start-segment (cdr rect)))
	(end (end-segment (cdr rect))))
    (- (y-point start)
       (y-point end))))
(define (width rect)
  (let ((start (start-segment (car rect)))
	(end (end-segment (car rect))))
    (- (x-point end) (x-point start))))

;; 4

(define (p-cons x y)
  (lambda (m) (m x y)))
(define (p-car z)
  (z (lambda (p q) p)))
(define (p-cdr z)
  (z (lambda (p q) q)))

;; expansion:
;; (p-car (p-cons x y))
;; (p-car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; (p-cdr (p-cons x y))
;; ... etc

;; 5

(define (num-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (num-car num-cell)
  (define (iter num-cell count)
    (if (= 0 (remainder num-cell 2))
	(iter (/ num-cell 2) (+ count 1))
	count))
  (iter num-cell 0))
(define (num-cdr num-cell)
  (define (iter num-cell count)
    (if (= 0 (remainder num-cell 3))
	(iter (/ num-cell 3) (+ count 1))
	count))
  (iter num-cell 0))

;; 6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(add-1 (lambda (f) (lambda (x) x)))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f x)))
(define one
  (lambda (f) (lambda (x) (f x))))

(add-1 one)
(add-1 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (l-+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; ----------------------------------------

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
		  (/ 1.0 (lower-bound y)))))

;; 7

(define (make-interval a b) (cons a b))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

;; 8

;; if a and b are intervals, then the highest possible value of a - b will be the maximum
;; possible value of a minus the minimum possible value of b. the lowest possible value
;; will be the lower bound of a minus the upper bound of b

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
		 (- (lower-bound x) (upper-bound y))))

;; 9

(define (width interval)
  (/ (abs (- (upper-bound interval) (lower-bound interval))) 2))

;; assume we have interval x with upper bound a and lower bound b, and interval y with
;; bounds c and d. (x + y) has upper bound (a + c) and lower (b + d). the width of x
;; is (a - b)/2, and the width of y is (c - d)/2, but the width of (x+y) is
;; (a + c - (d + b))/2 = (a - b + c - d)/2, which is just width(x) + width(y).

;; however this does not hold true for multipliction. for example, if x is the interval
;; (1, -20) and y is (5, -10) then xy = (200, -100). the width relies on the maximum of
;; all the possible products of the bounds of the intervals. its explicit formula is
;; (max(1*5, 1*-10, ...) - min(1*5, 1*-10,...))/2. the width of x is (1 + 20)/2 and the
;; width of y is (5+10)/2, so clearly there is no way to get the width of xy as a function
;; of just those widths (as each interval bound might be needed separately, and the subtraction
;; removes that information).

;; 10

(define (div-interval x y)
  (if (and (<= 0 (upper-bound y)) (>= 0 (lower-bound y)))
      (error "division by interval that includes 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
		      (/ 1.0 (lower-bound y))))))
;; 11

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;; (define (mul-interval x y)
;;   (cond ((

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 12

(define (make-center-percent c p)
  (make-center-width c (* c p)))
(define (percent i)
  (/ (width i) (center i)))

;; 13

;; if all numbers are positive then two intervals will be: x=(a + p1*a, a - p1*a),
;; y=(b + p2*b, b - p2*b). xy will be ((a + p1*a)*(b+p2*b), (a-p1*a)(b-p2*b)) ==
;; (ab + a*p2*b + p1*a*b + p1*a*b*p2, a*b -a*p2*b - p1*a*b + p2*p1*a*b) ==
;; (approx, if p1*p2 is close to zero) (ab + abp2 + abp1, ab - abp2 - abp1)
;; and so p3 = (p1 + p2)

;; ----------------------------------------

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
 		       (div-interval one r2)))))
;; 14

;; yup

;; 15

;; it does only use each interval once. is this better?
;; it seems so. i think this is because currently if an interval is used more than
;; once in an expression the uncertainty is "double counted". each use of the interval
;; isn't tied to the others, so in once part of the expression it could "take on"
;; values inconsistent with the other occurrences

;; 16
;; ...
;; maybe i'll come back to this

;; ------------------------------------------------------------

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

;; 17

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; 18

(define (reverse items)
  (define (iter accum items)
    (if (null? items)
	accum
	(iter (cons (car items) accum) (cdr items))))
  (iter '() items))

;; 19

(define first-denomenation car)
(define except-first-denomination cdr)
(define no-more? null?)

;; not done

;; 20

(define (same-parity . l)
  (define (same-parity? n)
    (= (remainder n 2)
       (remainder (car l) 2)))
  (define (recurse items)
    (if (null? items)
	items
	(if (same-parity? (car items))
	    (cons (car items) (recurse (cdr items)))
	    (recurse (cdr items)))))
  (recurse l))

;; ----------------------------------------------------------------------
(define nil ())
(define ni nil)
(define (scale-list items factor)
  (if (null? items)
      items
      (cons (* (car items) factor)
	    (scale-list (cdr items)
			factor))))


(define (map-sicp f items)
  (if (null? items)
      nil
      (cons (f (car items))
	    (map f (cdr items)))))
;; 21

(define (square x)
  (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; 22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items nil))

;; because the first item in the list is consed in a pair with nil, and then consed
;; on as the second item in a pair with the newer item first. this builds up pairs
;; backwards.

;; interchanging the arguments will just create a list that starts will a nil but doesn't
;; end with one, so the list will be malformed/improper. 

;; 23

(define (for-each f l)
  (f (car l))
  (if (null? (cdr l))
      #t
      (for-each f (cdr l)))) 

;; --------------------------------------------------

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

;; 24



(list 1 (list 2 (list 3 4)))
;; (1 (2 (3 4)))

;; [1 -]->[2 -]->[3 4]

;;      .
;;     / \
;;    1   .
;;       / \
;;      2   .
;;         / \
;;        3   4

;; 25

(cdr (car (cdr (cdr (list 1 3 (cons 5 7) 9)))))
(car (car (list (list 7))))
(cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))

;; 26

(define x (list 1 2 3))
(define y (list 4 5 6))

;; (1 2 3 5 6)
;; ((1 2 3) 4 5 6)
;; ((1 2 3) (4 5 6))

;; 27

(define (deep-reverse x)
  (if (pair? x)
      (reverse (map deep-reverse x))
      x))

;; 28

(define (fringe t)
  (if (pair? t)
      (append (fringe (car t)) (fringe (cdr t)))
      (if (null? t)
	  t
	  (list t))))

;; 29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
	(total-weight struct)
	struct)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (if (pair? mobile)
      (and (= (branch-weight (left-branch mobile))
	      (branch-weight (right-branch mobile)))
	   (balanced? (branch-structure (left-branch mobile)))
	   (balanced? (branch-structure (right-branch mobile))))
      #t))

(define (make-mobile-d left right)
  (cons left right))
(define (make-branch-d length structure)
  (cons length structure))

;; left-branch and branch-structure previously used (car (cdr)) because the structures
;; included a final nil member. with pure cons cells they could be rewritten to just
;; aliases of cdr.

;; --------------------------------------------------

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

;; 30


(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

;; 31

(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))
;; 32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (append (car s) x)) rest)))))

;; this is a pretty simple idea. if s is a set and a is the set of all subsets
;; of s, and s' is (s + x), then p(s') is just a + {a + x: a in a}. because
;; adding a new element means every subset could potentially contain that element
;; and so if you already have a list of all old subsets a list of all subsets
;; containing the new element will just be those subsets plus the new element

(define (filter pred seq)
  (cond ((null? seq) ni)
	((pred (car seq))
	 (cons (car seq)
	       (filter pred (cdr seq))))
        (else (filter pred (cdr seq))))))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
	  (accumulate op init (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define enumerate-tree fringe)

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map sqaure (map fib (enumerate-interval 0 n)))))
(define (product-of-squares-of-odd-elements seq)
  (accumulate * 1 (map square (filter odd? seq))))

;; 33

(define (map-ac p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append-ac seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-ac sequence)
  (accumulate (lambda (x y) (+ x 1)) 0 sequence))

;; 34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
	      0
	      coefficient-sequence))
;; 35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;; 36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))
;; 37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; 38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3)) 		; 3/2
(fold-left / 1 (list 1 2 3))		; 1/6
(fold-right list nil (list 1 2 3))	; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))	; (((() 1) 2) 3)

;; commutativity

;; 39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; --------------------------------------------------

(define (flatmap f seq)
  (accumulate append nil (map f seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car par) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
			   (lambda (i)
			     (map (lambda (j) (list i j))
				  (enumerate-interval 1 (- i 1))))
			   (enumerate-interval 1 n)))))
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
	  seq))

;; 40

(define (unique-pairs n)
  (flatmap (lambda (x) (map (lambda (y) (list x y))
			    (enumerate-interval 1 (- x 1))))
	   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; 41

(define (unique-triples n)
  (flatmap (lambda (x)
	     (map (lambda (y) (cons x y))
		  (unique-pairs (- x 1))))
	   (enumerate-interval 1 n)))

(define (sum-to? seq n)
  (= n (accumulate + 0 seq)))

(define (triples-that-sum-to s n)
  (filter (lambda (x) (sum-to? x s)) (unique-triples n)))

;; 42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define (queen-at-column column board)
  (first-p board (lambda (x) (= (cadr x) column))))

(define (pair-eq? p1 p2)
  (and (= (car p1) (car p2))
       (= (cadr p1) (cadr p2))))
(define (remove-p p? item seq)
  (filter (lambda (x) (not (p? item x))) seq))
(define (first-p seq p?)
  (car (filter p? seq)))
(define (none-of p? seq)
  (accumulate (lambda (x y) (and (not (p? x)) y)) #t seq))
(define (abs-sub p1 p2)
  (map abs (map - p1 p2)))
(define (same? pair)
  (= (car pair) (cadr pair)))
(define (safe? k positions)
  (let ((checking (queen-at-column k positions)))
    (none-of
     (lambda (x)
       (or (same? (abs-sub checking x))
           (= (car (abs-sub checking x)) 0)))
     (remove-p pair-eq? checking positions))))


;; 43

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions) (safe? k positions))
;;          (flatmap
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-position
;;                     new-row k rest-of-queens))
;;                  (enumerate-interval 1 board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))

;; this causes queen-cols to be called with the same number once for every item in the
;; interval, as opposed to the original which creates a new interval for every item in
;; queen-cols. as creating the interval is a linear process it is much faster for it to
;; be duplicated than for the entire recursive call to be called with the same arguments
;; many times. 

;; if the original queens function takes time t to run at size s, then rather than including
;; s further calls to queen-cols (total), there will be s further calls, each of which
;; will spawn (s-1) further calls, and so on. this results in s! further calls. if s calls took
;; time t then s! calls should take t(s-1)! time.

;; 44

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; 45

(define (split f1 f2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (f1 painter (f2 smaller smaller)))))
  iter)

;; ----------------------------------------

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

;; 46

(define (make-vect x y)
  (list x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cadr v))
(define (add-vect v1 v2)
  (map + v1 v2))
(define (sub-vect v1 v2)
  (map - v1 v2))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
;; 47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (edge2-frame frame)
  (cddr frame))


;; --------------------------------------------------

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

;; 48

(define (make-segment start end)
  (list start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))

;; 49

(define outline-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond-painter
  (segments->painter
   (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

;; how the fuck do i define wave?

;; ----------------------------------------------------------------------

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 0.5 1)
                     (make-vect 1 0.5)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0 0)
            split-point
            (make-vect 0 1)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1 0)
            (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; 50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

;; or

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

;; 51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (make-vect 0 0)
            (make-vect 1 0)
            split-point))
          (paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 0 1)
            (make-vect 1 0.5))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate90 (besides painter2 painter1)))

;; 52

;; a

;; no way am i gonna do all that number-fiddlin', i'm here for the crystallin abstract shit

;; b

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up right))
              (bottom-right (below right up))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;; c

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))
;; --------------------------------------------------------------------------------

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; 53

;; (a b c)
;; ((george))
;; (y1 y2)
;; y1
;; #f
;; #f
;; (red shoes blue socks)

;; 54

(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((not (or (pair? a) (pair? b)))
         (eq? a b))
        (else #f)))

;; 55

;; (car ''abracadabra) is equivilent to (car (quote (quote abracadabra))), or
;; (car (quote abracadabra)), so it of course evaluates to 'quote.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unknown expression type: " exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define (make-product m1 m2)
  (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2) (* m1 m2)))
        (else (list '* m1 m2))))

;; 56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                        (deriv (base exp) var))))
        (else
         (error "unknown expression type: " exp))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))
(define (exponentiation? exp)
  (eq? (car exp) '**))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))

;; 57

(define (addend s) (cadr s))
(define (augend s) (apply make-sum (cddr s)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (apply make-product (cddr p)))

(define (make-sum . s)
  (let ((numbers (filter number? s))
        (exps (filter (lambda (x) (not (number? x))) s)))
    (let* ((sum (accumulate + 0 numbers))
           (final (if (= 0 sum)
                      exps
                      (append exps (list sum)))))
      (if (= (length final) 1)
          (car final)
          (append (list '+) final)))))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product . s)
  (let ((numbers (filter number? s))
        (exps (filter (lambda (x) (not (number? x))) s)))
    (let* ((prod (accumulate * 1 numbers))
           (final (if (= prod 1)
                      exps
                      (append exps (list prod)))))
      (if (= prod 0)
          0
          (if (= (length final) 1)
              (car final)
              (append (list '*) final))))))

;; 58

;; a

(define (make-sum a b)
  (cond ((and (number? a) (number? b)) (+ a b))
        ((=number? a 0) b)
        ((=number? b 0) a)
        (else (list a '+ b))))
(define (addend s)
  (car s))
(define (augend s)
  (caddr s))

(define (sum? s)
  (eq? (cadr s) '+))

(define (make-product a b)
  (cond ((and (number? a) (number? b)) (* a b))
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((or (=number? a 0) (=number? b 0)) 0)
        (else (list a '* b))))
(define (multiplier s)
  (car s))
(define (multiplicand s)
  (caddr s))
(define (product? s)
  (eq? (cadr s) '*))

;; b

;; shit

;; ------------------------------------------------------------

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; 59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))

;; 60

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;; element-of-set? will take the same (or less) time. adjoin-set no longer has to
;; check for membership, and neither does union-set. intersection stays exactly
;; the same, but will be slower on this implementation as it will have to check multiple
;; of the same element.

;; so this should be used if you're doing a lot of unions and adjoins

;; --------------------------------------------------

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (id (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((<x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; 61

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))))

;; 62

(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
      (append set1 set2)
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond 
         ((= x1 x2)
          (cons x1 (union-set (cdr set1) (cdr set2))))
         ((< x1 x2)
          (cons x1 (union-set (cdr set1) set2)))
         ((> x1 x2)
          (cons x2 (union-set set1 (cdr set2))))))))

;; ------------------------------------------------------------

(define (entry tree) (car tree ))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((> x (entry set
         (element-of-set? x (right-branch set)))
        ((< x (entry set))
         (element-of-set? x (left-branch set)))))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; 63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;; a

;; no, tree->list-1 produces lists of trees with the flattened left branch first, followed
;; by entry and right branch, whereas tree->list-2 procudes lists with the entry and right
;; branch first.
;; (1 2 3 4 5 6 7)
;; (1 2 3 4 5 6 7)

;; b

;; the first function appends the left branch to the combination of the entry and the right
;; branch. since the tree is assumed to be balanced the total time is twice the time the
;; function would take for a tree with half the elements (the branches) plus the time taken
;; for the append of the half-sized list (linear).

;; so t(n) = 2t(n/2) + n/2
;; t(n) = 2 (2 t(n/4) + n/4) + n/2
;; t(n) = 2 (2 (2 t(n/8) + n / 8) + n/4) + n/2
;; t(n) = 2(4t(n/8) + n/4 + n/4) + n/2
;; t(n) = 8t(n/8) + n/2 + n/2 + n/2
;; ...
;; t(n) = 2^k*t(n/2^k) + k*(n/2)
;; since n is the number of leaves in a balanced binary tree we can assume it is of the
;; form 2^m. thus we can assume that log(n) = m \in \mathbf{n}. so if we let k = log(n):
;; t(n) = 2^log(n) * t(n/2^log(n)) + log(n)*(n/2)
;; t(n) = n * t(1) + 1/2 * nlog(n)
;; t(1) is a constant, as is 1/2, so both can be ignored for the purpose of big-oh
;; t(n) = n + n*log(n)
;; and nlogn dominates, so tree->list-1 \in o(n*log(n))

;; whereas the second function has t(n) = 2t(n/2) + c
;; or t(n) = 2(2t(n/4) + c) + c = 2(2(2t(n/8) + c) + c) + c = 8t(n/8) + 7c
;; so t(n) = 2^kt(n/2^k) + (2^k - 1)c, so if k=log(n):
;; t(n) = n*t(1) + nc - c = n(t(1) + c) - c, and so the constants are ingored
;; and t(n) = o(n)
;; so the second procedure has slower growth.

;; 64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
;; a
;; partial tree works by first by finding an integer that is 1 or 2 less than half n
;; then it finds the partial tree based on that length
;; but since this works by producing a tree followed by the unused elements the
;; actual tree is the car of this result
;; and the list of remaining elements is the cdr
;; so if the left tree is the first half-ish of the list then the size of the right tree is
;; the total size minus the left size (minus 1 because of the entry)
;; and the entry (or root) is the first item of the non-lefts
;; (since th elts are ordered the first left-size are all less than the entry, and all
;; after are greater)
;; and the "right result" is the rest of the non-lefts partial-treed
;; and the final result is the tree with the entry as the entry, then the left tree, then the right
;; of course consed to any remaining elements
;; this algorithm bottoms out with a list of 3, which gives a left size of 1

;; the tree should be:

;; length of list is 6
;; left-length is 2
;; entry is 5
;; right-size is 3
;;
;;    5

;; legth of left side is 2
;; left-length is 0
;; left branch is nil
;; entry is 1
;; right branch is 3

;;   5
;;  / \
;; 1
;;  \
;;   3

;; len of right side is 3
;; left-length is 1
;; entry is 9
;; right-length is 1

;;       5
;;      / \
;;     1   9
;;      \
;;       3

;; nil on right and left
;; so final is:


;;       5
;;     /   \
;;    1     9
;;     \   / \
;;      3 7  11

;; b

;; t(n) = t(n/2) + t(n/2) = 2*t(n/2) = o(n) for proper lists (see "proofs" above)

;; 65

;; seems like the easiest way is to convert the tree to a list, union/intersection with the
;; old algorithms, then convert back, for 3*o(n) = o(n).

;; so:

(define (tree-union-set set1 set2)
  (list->tree
   (union-set (tree->list-2 set1) (tree->list-2 set2))))

(define (tree-intersection-set set1 set2)
  (list->tree
   (intersection-set (tree->list-2 set1) (tree->list-2 set2))))

;; 66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records)))
         (car set-of-records))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (left-branch set-of-records)))))

;; ----------------------------------------------------------------------

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? obj) (eq? (car obj) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
     (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: choose-branch" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; 67

(define sample-tree
  (make-code-tree (make-leaf 'a 4)
                  (make-code-tree
                   (make-leaf 'b 2)
                   (make-code-tree
                    (make-leaf 'd 1)
                    (make-leaf 'c 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; (a d a b b c a)

;; 68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-list? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-list? x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-list? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-list? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree" symbol))))

;; this does encode to the correct message.

;; 69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((= (length leaf-set) 1) leaf-set)
        ((= (length leaf-set) 2)
         (make-code-tree (car leaf-set) (cadr leaf-set)))
        (else (successive-merge (adjoin-set
                                 (make-code-tree
                                  (car leaf-set) (cadr leaf-set))
                                 (cddr leaf-set))))))

;; 70

(define 1950s-tree (generate-huffman-tree
                    '((a 2) (get 2) (sha 3) (wah 1)
                      (boom 1) (job 2) (na 16) (yip 9))))
(define 1950-message
  '(get a job sha na na na na na na na na
        get a job sha na na na na na na na na
        wah yip yip yip yip yip yip yip yip yip
        sha boom))

;; encodes to a total of 84 bits. if this were a fixed length encoding, with 8 symbols
;; we would need 3 bits per symbol. the length of the message is 36. so we would need
;; 3*36 = 108 bits to encode the message.

;; 71

;; n = 5:
;; (1 2 4 8 16)
;; combine lowest two:
;; ((1 2 3) 4 8 16)
;; (((1 2 3) 4 7) 8 16)
;; ...
;;   .
;;  / \
;; 16  .
;;    / \
;;   8   .
;;      / \
;;     4   .
;;        / \
;;       2   1

;; same for any higher numbers. 1 bit will be required for the most frequent number and
;; n-1 bits for the least. this is because the sum of 2^i for i from 0 to k is 2^{k+1} -1
;; and so will always be less than the next one.

;; 72

;; with the encoding tree from 71 we have t(n) of encoding the most frequent as:
;; o(n) (element-of-list? on left) + o(1) (element of list on right, list is length
;; 1 and so time is constant) + c (a constant based on the checking of whether a list
;; is a leaf and the consing of nil). so total of o(n).

;; for the least frequent we have:
;; t(n) = t(n-1) + n + c (the rest of the tree, the element-of-list? and the checking the leaf)
;; t(n) = (t(n-2) + (n - 1) + c) + n + c
;; t(n) = ((t(n-2) + (n-2) + c) + (n-1) + c) + n + c
;; t(n) = t(n-k) + (k+1)(n+c) - (k(k+1)/2)
;; so if k = n-1:
;; t(n) = t(1) + (n-1+1)(n+c) - ((n-1)(n-1+2))/2
;; t(n) = t(1) + n^2 + nc - (n-1)n/2
;; t(n) = t(1) + n^2 + nc -(n^2 - n)/2
;; n^2 dominates here, so t(n) is o(n^2)

;; --------------------------------------------------------------------------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged datum: type-tag" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad tagged datum: contents" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "unknown type: real-part" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "unknown type: imag-part" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "unknown type: magnitude" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "unknown type: angle" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "no method for these types: apply-generic"
           (list op type-tags))))))

(define (deriv exp var)
  (cond ((number? exp 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp))
                (operands exp) var)))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; 73
;; a
;; because if exp is a number or a variable (a symbol) then it won't have a car
;; or cdr, and thus operator and operands won't work

;; b
(define (deriv-sum exp var)
  (make-sum (list (multiplier exp)
                       (deriv (multiplicand exp) var))))
(put 'deriv '(+) deriv-sum)
(define (deriv-prod exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (multiplicand exp)
                 (deriv (multiplier exp) var))))
(put 'deriv '(*) deriv-prod)
;; c
(define (deriv-exponent exp var)
  (make-product (exponent exp)
                       (make-product
                        (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                        (deriv (base exp) var))))
(put 'deriv '(**) deriv-exponent)
;; d
;; presumably the (put) lines would have to be rewritten to transpose the op and type

;; 74

;; a
(define (get-record employee file)
  ((get 'get-record (type-tag file)) employee file))
;; this will work so long as each file has a type tag (so has a type as its car)
;; and a specific procedure for retriving the data from the file.

;; b

(define (get-salary employee file)
  ((get 'get-salary file) employee))

;; c

(define (find-employee-record divisions employee)
  (car (map (lambda (x) (get-record employee x)) divisions)))

;; d

;; the procedures in use for getting records must be added to the table with put

;; --------------------------------------------------------------------------------

;; 75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else (error "unkonw op: " op))))
  dispatch)

;; 76

;; for explicit dispatch, the generic function will have to be modified for each new
;; data structure added.

;; for data-directed, each new function/data type will require a single put call.

;; for message passing a new function will require modification of the original object
;; constructor.
;; a new data object will require a whole new representation.


;; ----------------------------------------------------------------------

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (make-rat n d)
    (let* ((g (gcd n  d))
           (n (/ n g))
           (d (/ d g)))
      (if (negative? d)
          (cons (- n) (- d))
          (cons n d))))
  (define (numer rat) (car rat))
  (define (denom rat) (cdr rat))
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

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  (get 'make 'rational) n d)

;; 77

;; apply-generic takes an object and a tag and finds the appropriate function.
;; so applying it to x will give a call to get complex and magnitude as arguments.
;; since complex is not in the table this is an error. however with those added puts
;; it will first take in x, look for its associated function, find the same function
;; as the result, and pass x with the type tag stripped to that function.

;; so apply-generic will be called once to strip the complex tag, once more to strip the
;; rectangular tag, for a total of two times, at which point magnitude-rectangular is called

;; 78

(define (attach-tag tag object)
  (if (number? object)
      (object)
      (cons tag object)))
(define (type-tag object)
  (if (number? object)
      'scheme-number
      (car object)))
(define (contents object)
  (if (number? object)
      object
      (cdr object)))

;; 79

(define (equ? n1 n2)
  ((get 'equ? (list (type-tag n1) (type-tag n2))) n1 n2))
(put 'equ? '(scheme-number scheme-number) =)
(put 'equ? '(rational rational) equal-rat?)
(put 'equ? '(complex complex)
     (lambda (x y) (and (= (real-part x) (real-part y))
                        (= (imag-part x) (imag-part y)))))

;; 80
(define (=zero? n)
  (apply-generic '=zero? n))

(put '=zero? '(scheme-number) zero?)
(put '=zero? '(rational)
     (lambda (x) (= (numer x) 0)))
(put '=zero? '(complex)
     (lambda (x)
       (= 0 (real-part x) (imag-part x))))

;; ----------------------------------------------------------------------
(define (put . a) nil)
;; ----------------------------------------------------------------------

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;; 81

;; a

;; we'll enter an infinite loop. apply-generic wont' find a function that works on
;; the complex types and so finds functions that convert the first type to the
;; second and the second to the first. since functions are found for both
;; (just the identity type function) it will then apply-generic again with
;; the first type converted. since this doesn't actually change anything this process will
;; repeat forever

;; b

;; no, there is no need to change anything. the current function will raise a method not
;; found error if no conversion methods are found for the given types, which is the desired
;; behaviour if there is no function that works on those types.

;; c

;; just add a check and another error

;; 82

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (no-method) (error "no method for these types"
                               (list op type-tags)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1) (get-coercion type2 type1))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (no-method)))))
              (no-method))))))

;; that stratagy won't work if there are multiple arguments of types that can't be
;; coerced to each other, ie if a function formats a number according to a format string
;; then coercing a natural up to a rational would be fine but coercing the string to a
;; rational would fail.

;; (define (all-pairs l)
;;   )

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (define (no-method) (error "no method for these types"
;;                                (list op type-tags)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (let ((conversions ())))

;; come back

;; 83

(define (install-raise)
  (define (raise-scheme-number n)
    (make-rational n 1))
  (put 'raise '(integer) raise-scheme-number)
  (define (raise-rational-number n)
    (/ (numer n) (denom n)))
  (put 'raise '(rational) raise-rational-number)
  (define (raise-real-number n)
    (make-complex-from-real-imag n 0))
  (put 'raise '(real) raise-real-number))
(define (raise n)
    (apply-generic 'raise n))

;; 84
(define (type-eq? . args)
  (accumulate (lambda (x y) (if (eq? (type-tag (car args)) x) y #f)) #t args))
(define (type-higher? a b)
  (if (type-eq? a b)
      false
      (let ((raise-a (get 'raise (list (type-tag a)))))
        (if (raise-a)
            (type-higher? (raise-a) b)
            true))))
(define (type-highest args)
  (accumulate (lambda (a b) (if (type-higher? a b) a b)) 0 args))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (no-method) (error "no method for these types"
                               (list op type-tags)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((highest-type (type-highest args)))
            (if (apply type-eq? type-tags)
                (no-method)
                (apply
                 apply-generic
                 (cons op
                       (map
                        (lambda (x)
                          (if (type-eq? x highest-type)
                              x (raise x)))
                        args)))))))))
;; 85
(define (install-project-package)
  (define (project-complex x)
    (real x))
  (put 'project 'complex project-complex)
  (define (project-real x)
    (make-rational
     (round (* x 100000))
     100000))
  (put 'project 'real project-real)
  (define (project-rational x)
    (round (/ (numer x) (denom x))))
  (put 'project 'rational project-rational))
(define (project x)
  (apply-generic 'project x))
(define (drop x)
  (let ((proj (get 'project (list (type-tag x)))))
    (if (proj)
        (let ((dropped (proj x)))
          (if (equ? (raise dropped) x)
              (drop x)
              x))
        x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (no-method) (error "no method for these types"
                               (list op type-tags)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (let ((highest-type (type-highest args)))
            (if (apply type-eq? type-tags)
                (no-method)
                (drop (apply
                 apply-generic
                 (cons op
                       (map
                        (lambda (x)
                          (if (type-eq? x highest-type)
                              x (raise x)))
                        args))))))))))
;; 86

;; come back

;; ----------------------------------------------------------------------






(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "polys not in same var: add-poly" (list p1 p2))))
  (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "polys not in same var: mul-poly" (list p1 p2))))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
b        (else
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms l1) l2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms l1 (rest-terms l2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms l1)
                              (rest-tersm l2)))))))))
(define (mul-terms l1 l2)
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))
(define (mul-term-by-all-terms t1 l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms l))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

;; 87
(define (poly-zero? p)
  (define (zero-terms? terms)
    (if (empty-termlist? terms)
        true
        (and (=zero? (coeff (first-term terms)))
             (zero-terms? (rest-terms terms)))))
  (zero-terms? (term-list p)))
(put '=zero? '(polynomial) poly-zero?)

;; 88
(define (install-negate)
  (put 'negate '(rational)
       (lambda (x) (mul x (make-rational -1 1))))
  (put 'negate '(integer)
       (lambda (x) (* -1 x)))
  (put 'negate '(complex)
       (lambda (x)
         (make-complex-from-real-imag
          (negate (real x))
          (negate (imag x)))))
  (put 'negate '(real)
       (lambda (x) (* -1 x)))
  (define (negate-poly polynomial)
    (define (neg-termlist termlist)
      (if (empty-termlist? termlist)
          termlist
          (let ((first (first-term termlist)))
            (adjoin-term (make-term (order first) (negate (coeff first)))
                         (rest-terms termlist)))))
    (make-poly (variable polynomial) (neg-termlist (termlist polynomial))))
  (put 'negate '(polynomial) negate-poly))
(define (negate x)
  (apply-generic 'negate x))

(put 'sub '(polynomial polynomial)
     (lambda (a b)
       (if (same-variable? (variable a) (variable b))
           (add a (negate b))
           (error "polys not in same var: mul-poly" (list a b)))))

;; 89

;; rest-term stay the same
(define (first-term-dense term-list)
  (make-term (- (length term-list) 1) (car term-list)))
;; (define (adjoin-term term term-list)
;;   (let ((total-order (sub (length term-list) 1)))
;;     (cond ((equ? (order term) total-order)
;;            )

;; i'm super unclear on exactly what adjoin-term means and where a term would need to
;; be inserted in this mode (the other representation just conses new terms without
;; reguard for order, can i always assume the new term is of a greater order than any
;; others?) i'll try to come back later.

;; 90

;; come back

;; 91

(define (div-terms l1 l2)
  (if (empty-termlist? l1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term l1))
            (t2 (first-term l2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) l1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms
                      (sub l1
                           (mul-term-by-all-terms
                            (make-term new-o new-c) l2))
                      l2)))
                (list (ajdoin-term (make-term new-0 new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (div-poly a b)
  (if (same-variable? a b)
      (let ((terms (div-terms (term-list a) (term-list b))))
        (list (make-polynomial (variable a) (car terms))
              (make-polynomial (variable a) (cadr terms)))
      (error "not same variable" (list a b)))))

;; 92

;; come back

;; 93

(define (install-rational-package)
  (define (make-rat n d)
    (cons n d))
  (define (numer rat) (car rat))
  (define (denom rat) (cdr rat))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

;; 94

(define (gcd-poly a b)
  (if (same-variable? a b)
      (make-poly (variable a) (gcd-terms (term-list a) (term-list b)))
      (error "not same variable: gcd-poly" (list a b))))
(put 'greatest-common-divisor '(polynomial polynomial) gcd-poly)
(put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

;; 95

;; p1 := x^2 - 2x + 1
;; p2 := 11x^2 + 7
;; p3 := 13x + 5
;; q1 = 11x^4 - 22x^3 + 11x^2 + 7x^2 - 10x + 7 = 11x^4 - 22x^3 + 18x^2 - 10x + 7
;; q2 := 13x^3 - 21x^2 + 3x + 5
;; gcd(q1, q2)

;; can't see what's happening, these functions can't actually be implemented

;; 96

;; a

(define (psuedoremainder-terms a b)
  (cadr (div-terms (mul-term-by-all-terms
                    (exp (coeff (first-term b))
                         (+ 1 (order (first-term a))
                            (order (first-term b))))
                    a)
                   b))
(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (psuedoremainder-terms a b))))

;; b
(define (map-termslist-terms f terms)
  (if (empty-termlist? terms)
      terms
      (adjoin-term (f (first-term terms)) (map-termslist-terms rest-terms))))
(define (map-termslist f terms)
  (if (empty-termlist? terms)
      '()
      (cons (f (first-term terms)) (map-termslist rest-terms))))
(define (gcd-terms a b)
  (if (empty-termlist? b)
      (let ((terms-gcd (apply gcd (append (map-termslist coeff a) (map-termslist coeff b)))))
        (map-termslist-terms (lambda (term)
                               (make-term (order term) (/ (coeff term) terms-gcd)))
                             a))
      (gcd-terms b (psuedoremainder-terms a b))))
;; 97
;; a
(define (reduce-terms n d)
  (let ((terms-gcd
         (apply gcd (append (map-termslist coeff n)
                            (map-termslist coeff d)))))
    (define (dv-terms terms)
      (map-termslist-terms (lambda (term)
                               (make-term (order term) (/ (coeff term) terms-gcd)))
                             terms))
    (list (div-terms n) (div-terms d))))
(define (reduce-poly n d)
  (if (same-variable? n d)
      (let ((result (reduce-terms (term-list n) (term-list d))))
        (list (make-poly (variable n) (car result))
              (make-poly (variable n) (cadr result))))
      (error "not same variable")))

  
;; b

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(polynomial polynomial) reduce-poly)
(put 'reduce '(scheme-number scheme-number) reduce-integers)
(define (reduce n d) (apply-generic 'reduce n d))
(define (make-rat n d)
  (let ((reduced (reduce n d)))))
