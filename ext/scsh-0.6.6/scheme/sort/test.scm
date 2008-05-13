;;; Little test harness, 'cause I'm paraoid about tricky code.
;;; It's scsh specific -- Scheme 48 random-number stuff & the mail-notification
;;; stuff.

(define r (make-random 42))
(define (rand n) (modulo (r) n))

;;; For testing stable sort -- 3 & -3 compare the same.
(define (my< x y) (< (abs x) (abs y)))

(define (unstable-sort-test v) ; quick & heap vs simple insert
  (let ((v1 (vector-copy v))
	(v2 (vector-copy v))
	(v3 (vector-copy v))
	(v4 (vector-copy v)))
    (quick-sort!   < v1)
    (quick-sort3!  - v1)
    (heap-sort!    < v2)
    (insert-sort!  < v3)
    (and (or (not (equal? v1 v2))
	     (not (equal? v1 v3))
	     (not (equal? v1 v4))
	     (not (vector-sorted? < v1)))
	 (list v v1 v2 v3 v4))))

(define (stable-sort-test v) ; insert, list & vector merge sorts
  (let ((v1 (vector-copy v))
	(v2 (vector-copy v))
	(v3 (list->vector (list-merge-sort! my< (vector->list v))))
	(v4 (list->vector (list-merge-sort  my< (vector->list v)))))
    (vector-merge-sort! my< v1)
    (insert-sort!       my< v2)
    (and (or (not (equal? v1 v2))
	     (not (equal? v1 v3))
	     (not (equal? v1 v4))
	     (not (vector-sorted? my< v1)))
	 (list v v1 v2 v3 v4))))

(define (do-test max-size)
  (let lp ((i 0))
    (let ((i (cond ((= i 1000)
		    (write-char #\.)
		    (force-output)
		    0)
		   (else (+ i 1))))
	  (v (random-vector (rand max-size))))
      (cond ((unstable-sort-test v) => (lambda (x) (cons 'u x)))
	    ((stable-sort-test   v) => (lambda (x) (cons 's x)))
	    (else (lp i))))))

(define (test-n-mail max-size)
  (let ((losers (do-test max-size))
	(email-address "shivers@cc.gatech.edu"))
    (run (mail -s "sort lost" ,email-address) (<< ,losers))))

(define (random-vector size)
  (let ((v (make-vector size)))
    (fill-vector-randomly! v (* 10 size))
    v))

(define (fill-vector-randomly! v range)
  (let ((half (quotient range 2)))
    (do ((i (- (vector-length v) 1) (- i 1)))
	((< i 0))
      (vector-set! v i (- (rand range) half)))))
