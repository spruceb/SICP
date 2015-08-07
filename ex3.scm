;; chapter 3
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unkown request: " m))))
  dispatch)

;; 1

(define (make-accumulator initial)
  (lambda (x)
    (begin (set! initial (+ initial x))
           initial)))

;; 2

(define (make-monitored f)
  (let ((count (make-accumulator 0)))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          (count 0)
          (begin (count 1)
                 (f x))))))
;; 3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "unkown request: " m)))
        (lambda (x) "incorrect password")))
  dispatch)

;; 4

(define (make-account balance password)
  (define access-count (make-accumulator 0))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (begin (set! access-count (make-accumulator 0))
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "unkown request: " m))))
        (lambda (x) (if (= (access-count 0) 7)
                        call-the-cops
                        (begin (access-count 1)
                               "incorrect password")))))
  dispatch)

;; ----------------------------------------------------------------------
(define random-init 10)
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

;; 5

(define (random-in-range low high)
  (let ((range (- high low)))
    (let ((range (- high low)))
      (+ low (random range)))))

(define (estimate-integral p? x1 x2 y1 y2 trials)
  (* (monte-carlo trials
                  (lambda ()
                    (p? (random-in-range x1 x2)
                        (random-in-range y1 y2))))
     (abs (- x1 x2))
     (abs (- y1 y2))))
(define (in-unit x y)
  (>= 1 (+ (square x) (square y))))
(define (est-pi trials)
  (estimate-integral in-unit -1.0 1.0 -1.0 1.0 trials))

;; 6

(define rand
  (let ((x random-init)) 
    (lambda (option)
      (cond ((eq? option 'generate)
             (set! x (random-update x))
             x)
            ((eq? option 'reset)
             (lambda (new)
               (set! x new)))
            (else (error "unknown option"))))))
;; 7

(define (make-joint account old-pwd new-pwd)
  (lambda (pwd m)
    (if (eq? pwd new-pwd)
        (account old-pwd m)
        (account pwd m))))
;; 8

(define f
  (let ((store 0))
    (lambda (x)
      (let ((old store))
        (set! store (+ store x))
        old))))

;; 9

;; recursive

;; (factorial 6)

;; 10

;; extra environment frame with an unchanging initial value

;; ----------------------------------------------------------------------

;; 11

;; g:{make-account}
;; e1:{balance:50, withdraw, deposit, dispatch}->g
;; e2:{m:deposit}->e1
;; e3:{amount:40}->e1
;; e4:{m:withdraw}->e1
;; e5:{amount:60}->e1
;; local state is kept in e1
;; they are distinct because that call will create an entirly new frame containing a seprate
;; balance
;; the shared structure between the two is the code to be evaluated and the reference to global

;; 12

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

;; (b)
;; (b c d)

;; 13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; [a -]-> [b -]-> [c -]-|
;; ^----------------------

;; stack overflow/infinite recursion. there is no last pair

;; 14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; it destructivly reverses x by copying it into y (which is empty)

;; 15

;; meh

;; 16

;; returns 3

(cons (cons 'a 'b) (cons 'c 'd))

;; returns 4

;; [| -]->['a 'b]
;;  \/      ^
;; ['a -]---|

;; return 7

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x (cons 1 2))
(define y (cons x x))
(define z (cons y y))

;; infinite

(define x (cons 1 2))
(define y (cons 3 x))
(define z (cons 5 y))
(set-cdr! x z)

;; 17

(define (already-seen)
  (let ((seen-list '()))
    (define (eq-in? item list)
      (cond ((null? list) #f)
            ((eq? item (car list)) #t)
            (else (eq-in? item (cdr list)))))
    (lambda (x)
      (if (eq-in? x seen-list)
          #t
          (begin (set! seen-list (cons x seen-list))
                 #f)))))

(define (count-pairs x)
  (define (recurse x seen-function)
    (if (or (not (pair? x)) (seen-function x))
        0
        (+ (recurse (car x) seen-function)
           (recurse (cdr x) seen-function)
           1)))
  (recurse x (already-seen)))
  
;; 18

(define (cycle? lst)
  (define (recurse lst seen-fn)
    (cond ((null? lst) #f)
          ((seen-fn (cdr lst)) #t)
          (else (recurse (cdr lst) seen-fn))))
  (recurse lst (already-seen)))

;; 19
;; other way i've read about, only possible because of that reddit thread, i'm not this smart

(define (tortoise-and-hare-cycle? lst)
  (define (recurse lst1 lst2)
    (if (or (null? lst1) (null? lst2) (null? (cdr lst2))) #f
        (let ((next1 (cdr lst1)) (next2 (cddr lst2)))
          (if (eq? next1 next2) #t
              (recurse next1 next2)))))
  (recurse lst lst))

;; this is constant space, even if it seems like two copies of lst are being made
;; it's actually just two pointers to lst, and while the pointers change nothing else does,
;; so the space is just the cost of the two pointers (plus two more for the nexts)

;; 20

;; x is bound to a dispatch, which is evaluated in the environment:

;; {x: 1, y: 2}

;; z is also bound to a dispatch, this time in an environment:

;; {x: |, y: |}
;;     ---------- [dispatch -]--->{x: 1, y: 2}

;; (set-car! (cdr z) 17) is really just:
;; (((cdr z) 'set-car!) 17)

;; (cdr z) is (z 'cdr) is:
;; 


;; come back

;; 21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons () ()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "delete! empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;; 21

;; the interpreter doesn't care about pointers when printing. it just prints the car of a cell
;; followed by its cdr, unless the cdr is null in which case it ends the list.

;; in this case, q1 is initially (make-queue), which prints as (()). this seems like a list
;; containing nil, and it is, but since lists are simply cons cells with the final cdr being
;; nil it is also (() . ()), or a cons cell with both car and cdr being nil.

;; when a is inserted it is the first item inserted into the cell, which means both the
;; front and back pointers are set to the same cell, that cell being (a . ()). the queue
;; is set to a cell with both the car and cdr pointing to the a cell, or ((a . ()) . (a . ())).
;; because the interpreter prints lists instead of cells, this is seen as a list with the first
;; element being a list of one item (a) and the second item just being a, or ((a) a).

;; when b is inserted the rear-ptr is set to a new cell conting b as the car and nil as the cdr.
;; also, the previous rear-cell has its cdr set to the new cell instead of nil. however the
;; previous cell is also the front pointer. thus a list is constructed. the first poiinter was
;; (a . ()) and now is (a . (b . ())), and the last pointer was (a . ()) but now is (b . ()).
;; crucially the b cell is the same cell in both the front and back. thus when printing the queue
;; scheme prints the car first, which happens to be a list containing both a and b, then
;; the car of the cdr. the cdr is in fact the last cell in the list in the first cell, and so
;; b is printed again.

;; when a is removed the cdr of the first cell becomes the first cell, thus the last cell is also
;; the first.

;; and finally, deleting only changes the first pointer, it doesn't change the cell it points to.
;; ie, if you store a pointer to the then even after deleting everything in the queue
;; the whole list will still be there. for the queue to be empty the only requirement is that
;; the car be nil.

(define (print-queue queue)
  (display (front-ptr queue)))

;; 22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (front-queue)
      (if (null? front-ptr)
          (error "null front")
          (car front-ptr)))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))
        dispatch))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "delete on empty queue"))
            (else (set! front-ptr (cdr front-ptr))
                  dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'front-queue) (front-queue))
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m insert-queue!) insert-queue!)
            ((eq? m delete-queue!) delete-queue!)))
    dispatch))

;; 23

(define (make-deque)
  (cons '() '()))
(define (front-deque-ptr deque)
  (car deque))
(define (rear-deque-ptr deque)
  (cdr deque))
(define (dcell-item dcell)
  (car dcell))
(define (front-dcell-ptr dcell)
  (cadr dcell))
(define (rear-dcell-ptr dcell)
  (cddr dcell))
(define (set-dcell-front! dcell value)
  (set-car! (cdr dcell) value))
(define (set-dcell-rear! dcell value)
  (set-cdr! (cdr dcell) value))
(define (set-front-ptr! deque value)
  (set-car! deque value))
(define (set-rear-ptr! deque value)
  (set-cdr! deque value))
(define (front-deque deque)
  (dcell-item (front-deque-ptr deque)))
(define (rear-deque deque)
  (dcell-item (rear-deque-ptr deque)))
(define (empty-deque? deque)
  (or (null? (front-deque-ptr deque)) (null? (rear-deque-ptr deque))))
(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() (front-deque-ptr deque)))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-dcell-front! (front-deque-ptr deque) new-pair)
           (set-front-ptr! deque new-pair)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons (rear-deque-ptr deque) '()))))
    (cond ((empty-deque? deque)
           (front-insert-deque! deque item))
          (else
           (set-dcell-rear! (rear-deque-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))
           
(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "delete on empty deque")
      (let ((new-front (rear-dcell-ptr (front-deque-ptr deque))))
        (cond ((not (null? new-front))
               (set-dcell-front! new-front '())))
        (set-front-ptr! deque new-front)
        deque)))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "delete on empty deque")
      (let ((new-rear (front-dcell-ptr (rear-deque-ptr deque))))
        (cond ((not (null? new-rear))
               (set-dcell-rear! new-rear '())))
        (set-rear-ptr! deque new-rear)
        deque)))
;; ----------------------------------------------------------------------

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;; 24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "unknown table operation" m))))
    dispatch))

;; 25

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup keylist)
      (define (recurse table keys)
        (if (null? keys)
            (cdr table)
            (let ((next (assoc (car keys) (cdr table))))
              (if next
                  (recurse next (cdr keys))
                  false))))
      (recurse local-table keylist))
    
    (define (insert! keylist value)
      (define (recurse table keys)
        (let ((next (assoc (car keys) (cdr table))))
          (if next
              (if (null? (cdr keys))
                  (set-cdr! next value)
                  (recurse next (cdr keys)))
              (if (null? (cdr keys))
                  (set-cdr! table
                            (cons (cons (car keys) value)
                                  (cdr table)))
                  (let ((next-table (list (car keys))))
                    (set-cdr! table
                              (cons next-table
                                    (cdr table)))
                    (recurse next-table (cdr keys)))))))
      (recurse local-table keylist)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "unknown table operation" m))))
    dispatch))

(define (lookup keylist table)
  ((table 'lookup-proc) keylist))
(define (insert! keylist value table)
  ((table 'insert-proc!) keylist value))

;; 26

;; assuming there's a procedure greater? that compares keys a and b and also an equal?
;; then building a tree would be relatively simple. just have a top node with ('*table* . rest)
;; with rest being a binary tree where each node is a list containing the key, the value, and
;; the left and right trees. lookup can be done by checking the right or left tree recursively
;; based on greater? and returning the value once the keys are equal?. insertion can be done
;; by descending the tree and once a node is reached where the "correct" node to descend into would
;; be nil then create the containing node (key value nil nil) and set the previously nil location
;; to that value.

;; 27

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))
;; come back

;; ----------------------------------------------------------------------

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (cond ((and (= a 1) (= b 1)) 1)
        (else 0)))

;; 28

(define (or-gate a1 a2 output)
  (define (or-action-proc)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-proc)
  (add-action! a2 or-action-proc)
  'ok)

(define (logical-or a1 a2)
  (if (or (= a1 1) (= a2 1))
      1
      0))

;; 29

;; use de morgan: not (a and b) <-> not a or not b so a or b <-> not (not a and not b)
(define (or-gate a1 a2 output)
    (let ((not-a (make-wire)) (not-b (make-wire)) (and-out (make-wire)))
      (inverter a not-a)
      (inverter b not-b)
      (and-gate not-a not-b and-out)
      (inverter and-out output)
      'ok))

;; the delay = 3* inverter-delay + and-delay

;; 30

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum-c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-list b-list s-list c)
  (if (null? a-list)
      'ok
      (let ((c-out (make-wire)))
        (full-adder (car a-list) (car b-list) (car s-list) c c-out)
        (ripple-carry-adder (cdr a-list) (cdr b-list) (cdr s-list) c-out))))
;; let o be or-delay, a be and-delay, and i be inverter-delay.
;; half-adder delay (h) is o + i + 2a
;; full-adder delay (f) is h + h + o
;; ripple-delay is n * f = n * (h + h + o) = n * (o + i + 2a + o + i + 2a + o) =
;; 3o*n + 2i*n + 4a*n

;; ----------------------------------------------------------------------

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "unknown operation: wire" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " new-value = ")
                 (display (get-signal wire)))))
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; 31

;; because these gates all modify the state of a wire, and sometimes the state is only going
;; to change once and by one action. for instance, if an and gate's action were not not
;; called as soon as it were attached and both wires were on at that point, then the
;; output would remain off until the state of one of the inputs changed by some other means.
;; so in this example the initial probes wouldn't actually cause any printing. rather once
;; input-1 is set to 1, sum and carry both print with time 0, then
;; come back

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "empty agenda")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;; 32

;; the wires accept actions by consing new actions to a list, so the first item was the last
;; to be added. when that wire changes it calls all actions in order of the list. in the case
;; of the and gate both actions call after-delay and so both add a set-signal! lambda to the
;; agenda. If a1 is 0 and a2 is 1, then a1 changes to 1, it calls the and-action-procedure
;; action. This adds a lambda to the agenda at after-delay + current-time. unfortunately this
;; lambda will set the and gate output to 1, because at the time and-action-procedure was called
;; a1 had just be changed to 1 and a2 was still 1, so both are on. then a2 will be set to 0,
;; causing a similar chain of events but one which will add a procedure to the agenda at the
;; same time as the previous procedure, but which will set the output to 0 rather than 1.
;; if the procedures are stored in a list, with the last added begin consed to the front, then
;; the a2 procedure will be called (setting the output to 0) and then the a1 will be called
;; (setting it to 1), and thus the and gate will have an incorrect result. if it were a
;; queue instead, the a1 procedure would be called first, setting the output to an incorrect
;; state, and the a2 procedure would then be called to rectify the situation.
;; of course this whole thing will only happen if the wires are changed in a specific order,
;; but considering the only order that's supposed to matter is the agenda this is a
;; serious bug (and thus the queue).

;; ----------------------------------------------------------------------


(define (celcius-farenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "unknown request: adder" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (Get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: multiplier"
                       request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "unknown request: constant" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'i-have-a-value) (process-new-value))
          ((eq? request 'i-lost-my-value) (process-forget-value))
          (else (error "unknown request: probe" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                 inform-about-no-value
                                 constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "unknown operation: connector"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; 33

(define (averager a b c)
  (let ((result (make-connector)) (two (make-connector)))
    (adder a b result)
    (constant 2 two)
    (multiplier c two result)
    'ok))

;; 34

;; it won't work in the other direction. if a has a value then m1 and m2 will also
;; have values, and so the product will always be set (or a contradiction). if a
;; is unset and just product is set then the multiplier will have insufficient information
;; to solve for m1 and m2, because it doesn't know to use the square root instead of
;; any of the other infinite number of options.

;; 35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: squarer"
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me)) 
        (if (has-value? a)
            (set-value b (square (get-value b)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  
  (define (me request)
    (cond ((eq? request 'i-have-a-value) (process-new-value))
          ((eq? request 'i-lost-my-value) (process-forget-value))
          (else (erro "unknown request: squarer" request))))
  (connect a me)
  (connect b m)
  me)

;; 36

;; come back

;; 37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celcius-farenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celcius-farenheit-converter C))

;; ----------------------------------------------------------------------

;; 38

;; a

