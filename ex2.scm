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

