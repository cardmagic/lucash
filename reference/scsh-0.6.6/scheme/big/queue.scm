; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Queues
; Richard's code with Jonathan's names.
;
;     Richard's names:     Jonathan's names (modified by popular demand):
;      make-empty-queue     make-queue
;      add-to-queue!	    enqueue!
;      remove-from-queue!   dequeue!

(define-record-type queue :queue
  (really-make-queue uid head tail)
  queue?
  (uid queue-uid)
  (head queue-head set-queue-head!)
  (tail queue-tail set-queue-tail!))

(define *queue-uid* 0)

(define (make-queue)
  (let ((uid *queue-uid*))
    (set! *queue-uid* (+ uid 1))	;potential synchronization screw
    (really-make-queue uid '() '())))


; The procedures for manipulating queues.

(define (queue-empty? q)
  ;; (debug-message "queue-empty?" (queue? q))
  (null? (queue-head q)))

(define (enqueue! q v)
  ;; (debug-message "enqueue!" (queue? q))
  (let ((p (cons v '())))
    (if (null? (queue-head q))	;(queue-empty? q)
        (set-queue-head! q p)
        (set-cdr! (queue-tail q) p))
    (set-queue-tail! q p)))

(define (queue-front q)
  ;; (debug-message "queue-front" (queue? q))
  (if (queue-empty? q)
      (error "queue is empty" q)
      (car (queue-head q))))

(define (dequeue! q)
  ;; (debug-message "dequeue!" (queue? q))
  (let ((pair (queue-head q)))
    (cond ((null? pair)	;(queue-empty? q)
	   (error "empty queue" q))
	  (else
	   (let ((value (car pair))
		 (next  (cdr pair)))
	     (set-queue-head! q next)
	     (if (null? next)
		 (set-queue-tail! q '()))   ; don't retain pointers
	     value)))))

; Same again, except that we return #F if the queue is empty.
; This is a simple way of avoiding a race condition if the queue is known
; not to contain #F.

(define (maybe-dequeue! q)
  ;; (debug-message "maybe-dequeue!" (queue? q))
  (let ((pair (queue-head q)))
    (cond ((null? pair)			;(queue-empty? q)
	   #f)
	  (else
	   (let ((value (car pair))
		 (next  (cdr pair)))
	     (set-queue-head! q next)
	     (if (null? next)
		 (set-queue-tail! q '())) ; don't retain pointers
	     value)))))

(define (on-queue? v q)
  ;; (debug-message "on-queue!" (queue? q))
  (memq v (queue-head q)))

; This removes the first occurrence of V from Q.

(define (delete-from-queue! q v)
  (delete-from-queue-if! q (lambda (x) (eq? x v))))

(define (delete-from-queue-if! q pred)
  ;; (debug-message "delete-from-queue-if!" (queue? q))
  (let ((list (queue-head q)))
    (cond ((null? list)
	   #f)
	  ((pred (car list))
	   (set-queue-head! q (cdr list))
           (if (null? (cdr list))
               (set-queue-tail! q '()))   ; don't retain pointers
	   #t)
	  ((null? (cdr list))
	   #f)
	  (else
	   (let loop ((list list))
	     (let ((tail (cdr list)))
	       (cond ((null? tail)
		      #f)
		     ((pred (car tail))
		      (set-cdr! list (cdr tail))
		      (if (null? (cdr tail))
			  (set-queue-tail! q list))
		      #t)
		     (else
		      (loop tail)))))))))

(define (queue->list q)        ;For debugging
  (map (lambda (x) x)
       (queue-head q)))

(define (queue-length q)
  (length (queue-head q)))
