; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Channel interrupt stuff.

; Install an interrupt handler that cells up the results of completed I/O
; operations and spawn a thread to cope with them.  This is written so as
; to avoid having state in top-level variables, because their values are
; saved in dumped images.

(define (initialize-channel-i/o!)
  (session-data-set! channel-wait-cells-slot '())
  (session-data-set! channel-wait-count-slot 0)
  (set-interrupt-handler! (enum interrupt i/o-read-completion)
			  (make-i/o-completion-handler
			   (lambda (cell channel)
			     (let ((old (cell-ref cell)))
			       (cell-set! cell
					  (cons (cons channel (car old))
						(cdr old)))))))
  (set-interrupt-handler! (enum interrupt i/o-write-completion)
			  (make-i/o-completion-handler
			   (lambda (cell channel)
			     (let ((old (cell-ref cell)))
			       (cell-set! cell
					  (cons (car old)
						(cons channel (cdr old)))))))))

; The warning message is printed using DEBUG-MESSAGE because to try to make
; sure it appears in spite of whatever problem's the I/O system is having.

(define (make-i/o-completion-handler update-ready-cell)
  ;; Called with interrupts disabled.
  (lambda (channel status enabled-interrupts)
    (call-with-values
     (lambda () (fetch-channel-wait-cell! channel))
     (lambda (thread-cell maybe-ready-cell)
       (cond
	((and thread-cell (cell-ref thread-cell))
	 => (lambda (thread)
	      (decrement-channel-wait-count!)
	      (make-ready thread status)))
	;; The problem with the debug message is that
	;; WAIT-FOR-CHANNELS can trigger this warning in a perfectly
	;; legimitate situation because of a race I don't know how to
	;; avoid. --Mike
;	(else
;	 (debug-message "Warning: dropping ignored channel i/o result {Channel "
;			(channel-os-index channel)
;			" "
;			(channel-id channel)
;			"}"))
	)
       (cond
	((and maybe-ready-cell
	      (cell-ref maybe-ready-cell))
	 (update-ready-cell maybe-ready-cell channel)))))))

; Exported procedure

(define (waiting-for-i/o?)
  (< 0 (channel-wait-count)))

; Block until the current I/O operation on CHANNEL has completed.
; This returns the result of the operation.
;
; This needs to be called with interrupts disabled.
;
; We install a DYNAMIC-WIND to abort the operation if the waiting thread is
; terminated.

(define (wait-for-channel channel)
  (call-with-values
   (lambda () (fetch-channel-wait-cell! channel))
   (lambda (thread-cell maybe-ready-cell)
     (if (and thread-cell (cell-ref thread-cell))
	 (begin
	   (add-channel-wait-cell! channel thread-cell #f)
	   (warn "channel has two pending operations" channel)
	   (terminate-current-thread))
	 (let ((cell (make-cell (current-thread))))
	   (increment-channel-wait-count!)
	   (set-thread-cell! (current-thread) cell)
	   (add-channel-wait-cell! channel cell #f)
	   (dynamic-wind nothing
			 block
			 (lambda ()
			   (with-interrupts-inhibited
			    (lambda ()
			      (if (cell-ref cell)
				  ;; we're being terminated
				  (begin
				    (fetch-channel-wait-cell! channel)
				    (channel-abort channel)
				    (wait-for-channel channel))))))))))))

(define (nothing) #f)

(define (channel-check-waiter channel)
  (if (channel-has-waiter? channel)
      (begin
	(warn "channel has two pending operations" channel)
	(terminate-current-thread))))

(define (wait-for-channels read-channels write-channels timeout)
  ;; check if we're borked from the outset
  (for-each channel-check-waiter read-channels)
  (for-each channel-check-waiter write-channels)

  (let ((thread-cell (make-cell (current-thread)))
	(ready-channels-cell (make-cell (cons '() '())))
	(ready-read-channels '())
	(ready-write-channels '()))

    (if (or (not timeout)
	    (register-dozer thread-cell timeout))
	(begin
	  ;; register us with every channel we're waiting for
	  (set-thread-cell! (current-thread) thread-cell)
	  (let ((signup (lambda (channel)
			  (add-channel-wait-cell! channel
						  thread-cell ready-channels-cell)
			  (increment-channel-wait-count!))))
	    (for-each signup read-channels)
	    (for-each signup write-channels))

	  ;; block
	  (dynamic-wind
	   nothing
	   (lambda ()
	     (block)
	     (disable-interrupts!)
	     (let ((pair (cell-ref ready-channels-cell)))
	       (set! ready-read-channels (car pair))
	       (set! ready-write-channels (cdr pair)))
	     (cell-set! ready-channels-cell #f)
	     (enable-interrupts!)
	     (values ready-read-channels ready-write-channels))
	   ;; clean up
	   (lambda ()
	     (let ((aborting? (and (cell-ref thread-cell) #t)))
	       (with-interrupts-inhibited
		(lambda ()
		  ;; this ain't so great ...
		  (let ((make-cleanup
			 (lambda (ready-channels)
			   (lambda (channel)
			     (if (not (memq channel ready-channels))
				 (begin
				   (fetch-channel-wait-cell! channel)
				   (if (not aborting?)
				       (decrement-channel-wait-count!)
				       (begin
					 (channel-abort channel)
					 (wait-for-channel channel)))))))))
		    (for-each (make-cleanup ready-read-channels) read-channels)
		    (for-each (make-cleanup ready-write-channels) write-channels))))))))
	;; the timeout was zero or less
	(enable-interrupts!))))

; Abort any pending operation on by OWNER on CHANNEL.
; Called with interrupts disabled.
  
(define (steal-channel! channel owner)
  (call-with-values
   (lambda () (fetch-channel-wait-cell! channel))
   (lambda (thread-cell maybe-ready-cell)
     (cond
      ((cell-ref thread-cell)
       => (lambda (thread)
	    (cond ((eq? thread owner)
		   (clear-thread-cell! thread)
		   (decrement-channel-wait-count!)
		   (channel-abort channel))
		  (else
		   (warn "channel in use by other than port owner" 
			 channel thread owner)
		   #f))))
      (else #f)))))

; Have CHANNEL-READ and CHANNEL-WRITE wait if a pending-channel-i/o
; exception occurs.

(define-exception-handler (enum op channel-maybe-read)
  (lambda (opcode reason buffer start count wait? channel . maybe-os-message)
    (if (= reason (enum exception pending-channel-i/o))
	(wait-for-channel channel)
	(begin
	  (enable-interrupts!)
 	  (apply signal-exception
 		 opcode reason buffer start count wait? channel
 		 maybe-os-message)))))

(define-exception-handler (enum op channel-maybe-write)
  (lambda (opcode reason buffer start count channel . maybe-os-message)
    (if (= reason (enum exception pending-channel-i/o))
	(wait-for-channel channel)
	(begin
	  (enable-interrupts!)
 	  (apply signal-exception
 		 opcode reason buffer start count channel
 		 maybe-os-message)))))

; Two session slots
;   - the number of threads waiting for I/O completion events
;   - an alist mapping channels to cells for waiting threads

(define channel-wait-count-slot (make-session-data-slot! 0))

(define (channel-wait-count)
  (session-data-ref channel-wait-count-slot))

(define (increment-channel-wait-count!)
  (session-data-set! channel-wait-count-slot (+ (channel-wait-count) 1)))

(define (decrement-channel-wait-count!)
  (session-data-set! channel-wait-count-slot (- (channel-wait-count) 1)))

(define channel-wait-cells-slot (make-session-data-slot! '()))

; Adding a cell and channel - the caller has already determined there is no
; existing cell for this channel.

(define (add-channel-wait-cell! channel cell maybe-ready-channels-cell)
  (session-data-set! channel-wait-cells-slot
		     (cons (cons channel (cons cell maybe-ready-channels-cell))
			   (session-data-ref channel-wait-cells-slot))))

; This is just deleting from an a-list.

(define (fetch-channel-wait-cell! channel)
  (let* ((cells (session-data-ref channel-wait-cells-slot))
	 (cell+ready-channels-cell
	  (cond ((null? cells)
		 #f)
		((eq? channel (caar cells))
		 (session-data-set! channel-wait-cells-slot
				    (cdr cells))
		 (cdar cells))
		(else
		 (let loop ((cells (cdr cells)) (prev cells))
		   (cond ((null? cells)
			  #f)
			 ((eq? channel (caar cells))
			  (set-cdr! prev (cdr cells))
			  (cdar cells))
			 (else
			  (loop (cdr cells) cells))))))))
    (cond
     (cell+ready-channels-cell
      => (lambda (pair)
	   (let ((thread-cell (car pair))
		 (ready-cell (cdr pair)))
	     (values thread-cell ready-cell))))
     (else
      (values #f #f)))))

(define (channel-has-waiter? channel)
  (and (assq channel 
	     (session-data-ref channel-wait-cells-slot))
       #t))

