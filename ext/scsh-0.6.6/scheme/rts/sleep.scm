; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Sleeping for N milliseconds.

(define (sleep user-n)
  (let ((n (coerce-to-nonnegative-integer user-n)))
    (cond ((not n)
	   (call-error "wrong type argument" sleep user-n))
	  ((< 0 n)
	   (let ((cell (make-cell (current-thread))))
	     (disable-interrupts!)
	     (set-thread-cell! (current-thread) cell)
	     (insert-dozer! cell n)
	     (block))))))

(define (register-dozer cell user-n)
  (let ((n (coerce-to-nonnegative-integer user-n)))
    (cond ((not n)
	   (call-error "wrong type argument" sleep user-n))
	  ((< 0 n)
	   (insert-dozer! cell n)
	   #t)
	  (else #f))))

(define (insert-dozer! cell n) 
  (set! *dozers*
	(insert (cons (+ (real-time) n)
		      cell)
		*dozers*
		(lambda (frob1 frob2)
		  (< (car frob1) (car frob2))))))

(define (coerce-to-nonnegative-integer n)
  (if (real? n)
      (let* ((n (round n))
	     (n (if (exact? n)
		    n
		    (inexact->exact n))))
	(if (<= 0 n)
	    n
	    #f))
      #f))

(define *dozers* '())  ; List of (wakeup-time . cell)
	  
(define (insert x l <)
  (cond ((null? l) (list x))
	((< x (car l)) (cons x l))
	(else (cons (car l) (insert x (cdr l) <)))))

; Called by root scheduler, so won't be interrupted.
; This returns two values, a boolean that indicates if any threads were
; woken and the time until the next sleeper wakes.  We have to check for
; threads that have been started for some other reason.

(define (wake-some-threads)
  (if (null? *dozers*)
      (values #f #f)
      (let ((time (real-time)))
	(let loop ((dozers *dozers*) (woke? #f))
	  (if (null? dozers)
	      (begin
		(set! *dozers* '())
		(values woke? #f))
	      (let* ((next (car dozers))
		     (thread (cell-ref (cdr next))))
		(cond ((not thread)
		       (loop (cdr dozers) woke?))
		      ((< time (car next))
		       (set! *dozers* dozers)
		       (values woke? (- (car next) time)))
		      (else
		       (make-ready thread)
		       (loop (cdr dozers) #t)))))))))
