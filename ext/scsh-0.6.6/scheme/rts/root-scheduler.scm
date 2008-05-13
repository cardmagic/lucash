; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; The root scheduler.
;
; This uses RUN-THREADS-WITH-HOUSEKEEPER from the round-robin scheduler.
; The housekeeping thread flushes output buffers and wakes any sleeping
; threads whose time has come.

(define (root-scheduler thunk quantum housekeeping-quantum)
  (let ((*result* 111))
    (call-with-current-continuation
      (lambda (abort)
	(initialize-channel-i/o!)
	(run-threads-with-housekeeper (make-root-event-handler
				         (lambda () (set! *result* (thunk)))
					 quantum
					 abort)
				      (lambda ()
					(spawn-output-forcers #t)
					(wake-some-threads))
				      housekeeping-quantum)
	*result*))))

; Returns a handler and a procedure for adding new threads.  No events
; are handled specially.  The only upcall is for aborting execution.

(define (make-root-event-handler thunk quantum abort)
  (let ((runnable (make-queue))
	(thread-count (make-counter))
	(safe-dynamic-env (with-handler root-handler get-dynamic-env))
	(thread (make-thread thunk
			     (get-dynamic-env)
			     'scheduler-initial-thread)))
    (increment-counter! thread-count)
    (enqueue! runnable thread)
    (round-robin-event-handler
       runnable quantum safe-dynamic-env thread-count
       (lambda args #f)			; we handle no events
       (lambda (thread token args)	; upcall handler
	 (if (eq? token abort-token)
	     (abort (car args))
	     (propogate-upcall thread token args)))
       root-wait)))

; Let the user know if anything goes wrong while running a root thread.
; Errors kill the offending thread, warnings allow it to proceed.

(define (root-handler condition next-handler)
  (let ((out (current-error-port)))
    (cond ((error? condition)
	   (display "Error while running root thread, thread killed: " out)
	   (display (current-thread) out)
	   (newline out)
	   (cheap-display-condition condition out)
	   (terminate-current-thread))
	  ((warning? condition)
	   (cheap-display-condition condition out)
	   (unspecific))     ;proceed
	  (else
	   (next-handler)))))

(define (cheap-display-condition condition out)
  (display (case (car condition)
	     ((error) "Error")
	     ((exception) "Exception")
	     ((warning) "Warning")
	     (else (car condition)))
	   out)
  (display ": " out)
  (display (cadr condition) out)
  (newline out)
  (for-each (lambda (irritant)
	      (display "    " out)
	      (display irritant out)
	      (newline out))
	    (cddr condition)))
	 
; Upcall token

(define abort-token (list 'abort-token))

(define scheme-exit-now
   (lambda (status)
      (upcall abort-token status)))

; Getting around to calling the VM's WAIT procedure.  We disable interrupts
; to keep things from happening behind our back, and then see if there is
; any thread to run or any event pending, or if work may appear in the future.

(define (root-wait)
  (set-enabled-interrupts! 0)
  (let ((forcers? (spawn-output-forcers #f)))
    (call-with-values
     wake-some-threads
     (lambda (woke-some? time-until-wakeup)
       (cond ((or forcers? woke-some? (event-pending?))
	      (set-enabled-interrupts! all-interrupts)
	      #t)
	     ((or time-until-wakeup
		  (waiting-for-i/o?)
		  (waiting-for-sigevent?))
	      (do-some-waiting time-until-wakeup)
	      (set-enabled-interrupts! all-interrupts)
	      (root-wait))
	     ((session-data-ref deadlock-handler)
	      => (lambda (handler)
		   (handler)
		   (set-enabled-interrupts! all-interrupts)
		   #t))
	     (else
	      (set-enabled-interrupts! all-interrupts)
	      #f))))))

(define one-day-of-milliseconds (* (* 1000 60) (* 60 24)))

; A mess because a fixnum's worth of milliseconds is only a few days.
; The VM's WAIT procedure takes its maximum-wait argument in either
; milliseconds or minutes.

(define (do-some-waiting time-until-wakeup)
  (call-with-values
   (lambda ()
     (cond ((not time-until-wakeup)
	    (values -1 #f))
	   ((< time-until-wakeup one-day-of-milliseconds)
	    (values time-until-wakeup #f))
	   (else
	    (values (quotient time-until-wakeup 60000)
		    #t))))
   (structure-ref primitives wait)))

(define deadlock-handler (make-session-data-slot! #f))

(define (call-when-deadlocked! thunk)
  (session-data-set! deadlock-handler thunk))

; Find any ports that need to be flushed.  We get both a thunk to flush the
; port and the port itself; the port is only used for reporting problems.

(define (spawn-output-forcers others-waiting?)
  (let ((thunks (output-port-forcers others-waiting?)))
    (cond ((null? thunks)
	   #f)
	  (else
	   (for-each (lambda (thunk)
		       (spawn-on-root thunk 'output-forcer))
		     thunks)
	   #t))))
	 
(define unspecific (structure-ref primitives unspecific))
