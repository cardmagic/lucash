; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.
; Locks (= semaphores)

; Each lock has:
;   The owning thread's uid, or #f if not locked.  The uid can be used
;     to aid debugging without introducing the overhead of a weak pointer
;     to the actual thread (a non-weak pointer would introduce an unfortunate
;     circularity involving the locks and finalizers of ports).
;   A queue of waiting threads

(define-record-type lock :lock
  (really-make-lock owner-uid queue uid)
  lock?
  (owner-uid lock-owner-uid set-lock-owner-uid!)
  (queue lock-queue)
  (uid lock-uid))     ; for debugging

(define *lock-uid* 0)

(define (make-lock)
  (let ((uid *lock-uid*))
    (set! *lock-uid* (+ uid 1))
    (really-make-lock #f (make-queue) uid)))

(define (obtain-lock lock)
  (with-interrupts-inhibited
   (lambda ()
     (if (lock-owner-uid lock)
	 (block-on-queue (lock-queue lock))
	 (set-lock-owner-uid! lock (thread-uid (current-thread)))))))

(define (maybe-obtain-lock lock)
  (with-interrupts-inhibited
   (lambda ()
     (if (lock-owner-uid lock)
	 #f
	 (begin
	   (set-lock-owner-uid! lock (thread-uid (current-thread)))
	   #t)))))

(define (obtain-lock-multiple . all-locks)
  (with-interrupts-inhibited
   (lambda ()
     (let loop ((locks all-locks))
       (cond
	((null? locks)
	 (for-each (lambda (lock)
		     (enqueue-thread! (lock-queue lock) (current-thread)))
		   all-locks)
	 (block))
	((lock-owner-uid (car locks))
	 (loop (cdr locks)))
	(else
	 (set-lock-owner-uid! (car locks)
			      (thread-uid (current-thread)))))))))

; Returns #t if the lock has no new owner.

(define (release-lock lock)
  (with-interrupts-inhibited
   (lambda ()
     (let ((queue (lock-queue lock)))
       (cond
	((maybe-dequeue-thread! queue)
	 => (lambda (next)
	      (set-lock-owner-uid! lock (thread-uid next))
	      (make-ready next)
	      #f))
	(else
	 (set-lock-owner-uid! lock #f)
	 #t))))))
