; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.
; Placeholders (single-assignment cells for use with threads)

(define-record-type placeholder :placeholder
  (really-make-placeholder queue id)
  placeholder?
  (queue placeholder-queue set-placeholder-queue!) ; #f means VALUE has been set
  (value placeholder-real-value set-placeholder-value!)
  (id placeholder-id))

(define-record-discloser :placeholder
  (lambda (placeholder)
    (cons 'placeholder
	  (if (placeholder-id placeholder)
	      (list (placeholder-id placeholder))
	      '()))))

(define (make-placeholder . id-option)
  (really-make-placeholder (make-queue)
			   (if (null? id-option) #f (car id-option))))

(define (placeholder-value placeholder)
  (with-interrupts-inhibited
   (lambda ()
     (if  (placeholder-queue placeholder)
	  (block-on-queue (placeholder-queue placeholder)))
     (placeholder-real-value placeholder))))

(define (placeholder-set! placeholder value)
  (let ((waiters (with-interrupts-inhibited
		  (lambda ()
		    (let ((queue (placeholder-queue placeholder)))
		      (cond (queue
			     (set-placeholder-value! placeholder value)
			     (set-placeholder-queue! placeholder #f)
			     (let loop ((waiters '()))
			       (cond
				((maybe-dequeue-thread! queue)
				 => (lambda (thread)
				      (loop (cons thread waiters))))
				(else
				 waiters))))
			    (else #f)))))))
    (if waiters
	(for-each make-ready waiters)
	(if (not (eq? value (placeholder-value placeholder)))
	    (error "placeholder is already assigned"
		   placeholder
		   value)))))