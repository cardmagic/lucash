; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Ports built on OS channels.
;
; We wrap the channels in a record so that we can distinguish these ports
; from others.

; Repeatedly calls CHANNEL-READ until enough characters have been obtained.
; There are three different conditions under which the buffer-procedure
; is called:
;  1) any number of characters is fine, but an immediate response is
;     needed (the originating procedure was CHAR-READY?)
;  2) wait for at least one character, but not after that (from READ-CHAR)
;  3) exactly NEEDED characters are required (from READ-BLOCK)

(define (channel-port-ready? channel-cell)
  (channel-ready? (channel-cell-ref channel-cell)))

(define input-channel-handler
  (make-buffered-input-port-handler
   (lambda (channel-cell)
     (list 'input-port (channel-cell-ref channel-cell)))
   (lambda (channel-cell)
     ((channel-cell-closer channel-cell)
        (channel-cell-ref channel-cell)))
   (lambda (channel-cell buffer start needed)
     (channel-read buffer start needed (channel-cell-ref channel-cell)))
   channel-port-ready?
   (lambda (channel-cell owner)
     (steal-channel! (channel-cell-ref channel-cell) owner))))

(define (input-channel->port channel . maybe-buffer-size)
  (real-input-channel->port channel maybe-buffer-size close-input-channel))

; This is for sockets, which have their own closing mechanism.

(define (input-channel+closer->port channel closer . maybe-buffer-size)
  (real-input-channel->port channel maybe-buffer-size closer))
	     
(define (real-input-channel->port channel maybe-buffer-size closer)
  (let ((buffer-size (if (null? maybe-buffer-size)
			 default-buffer-size
			 (car maybe-buffer-size))))
    (if (>= 0 buffer-size)
	(call-error "invalid buffer size" input-channel->port channel buffer-size)
	(make-buffered-input-port input-channel-handler
				  (make-channel-cell channel closer)
				  (make-code-vector buffer-size 0)
				  0
				  0))))
  
(define output-channel-handler
  (make-buffered-output-port-handler
   (lambda (channel-cell)
     (list 'output-port (channel-cell-ref channel-cell)))
   (lambda (channel-cell)
     ((channel-cell-closer channel-cell)
        (channel-cell-ref channel-cell)))
   (lambda (channel-cell buffer start count)
     (channel-write buffer start count (channel-cell-ref channel-cell)))
   channel-port-ready?
   (lambda (channel-cell owner)
     (steal-channel! (channel-cell-ref channel-cell) owner))))

; Unbuffered channel output ports.  Used for the default error port.

(define (make-unbuffered-output-channel-handler)
  (let ((buffer (make-code-vector 1 0)))
    (make-port-handler
     (lambda (channel-cell)
       (list 'output-port (channel-cell-ref channel-cell)))
     (lambda (channel-cell)
       ((channel-cell-closer channel-cell)
          (channel-cell-ref channel-cell)))
     (lambda (channel-cell char)
       (code-vector-set! buffer 0 (char->ascii char))
       (channel-write buffer 0 1 (channel-cell-ref channel-cell)))
     (lambda (channel-cell)
       (channel-ready? (channel-cell-ref channel-cell)))
     (lambda (channel-cell owner)
       (steal-channel! (channel-cell-ref channel-cell) owner)))))

; Dispatch on the buffer size to make the appropriate port.  A buffer
; size of zero creates an unbuffered port.  Buffered output ports get a
; finalizer to flush the buffer if the port is GC'ed.

(define (output-channel->port channel . maybe-buffer-size)
  (real-output-channel->port channel maybe-buffer-size close-output-channel))

; This is for sockets, which have their own closing mechanism.

(define (output-channel+closer->port channel closer . maybe-buffer-size)
  (real-output-channel->port channel maybe-buffer-size closer))
	     
(define (real-output-channel->port channel maybe-buffer-size closer)
  (let ((buffer-size (if (null? maybe-buffer-size)
			 default-buffer-size
			 (car maybe-buffer-size))))
    (cond ((> 0 buffer-size)
	   (call-error "invalid buffer size"
		       output-channel->port channel buffer-size))
	  ((= 0 buffer-size)
	   (make-unbuffered-output-port (make-unbuffered-output-channel-handler)
					(make-channel-cell channel closer)))
	  (else
	   (let ((port (make-buffered-output-port 
			output-channel-handler
			(make-channel-cell channel closer)
			(make-code-vector buffer-size 0)
			0
			buffer-size)))
	     (periodically-force-output! port)
	     ((structure-ref primitives add-finalizer!) port
							maybe-force-output)
	     port)))))
	     
; Flush PORT's output buffer if the port is open and not locked.

(define (maybe-force-output port)
  (cond ((maybe-obtain-lock (port-lock port))
	 (report-errors-as-warnings (lambda ()
				      (really-force-output port))
				    "error while flushing GC'ed port's buffer"
				    port)
	 (release-lock (port-lock port)))))

;----------------
; The records we use to mark channel ports and the function that makes use of
; them.

(define-record-type channel-cell :channel-cell
  (make-channel-cell channel closer)
  channel-cell?
  (channel channel-cell-ref)
  (closer channel-cell-closer))

(define (port->channel port)
  (let ((data (port-data port)))
    (if (channel-cell? data)
	(channel-cell-ref data)
	#f)))

;----------------
; Various ways to open ports on files.

(define (open-input-file string)
  (if (string? string)
      (input-channel->port (open-input-channel string))
      (call-error "invalid argument" open-input-file string)))

(define (open-output-file string)
  (if (string? string)
      (output-channel->port (open-output-channel string))
      (call-error "invalid argument" open-output-file string)))

(define (call-with-input-file string proc)
  (let* ((port (open-input-file string))
         (results (call-with-values (lambda () (proc port))
				    list)))
    (close-input-port port)
    (apply values results)))

(define (call-with-output-file string proc)
  (let* ((port (open-output-file string))
         (results (call-with-values (lambda () (proc port))
				    list)))
    (close-output-port port)
    (apply values results)))

(define (with-input-from-file string thunk)
  (call-with-input-file string
    (lambda (port)
      (call-with-current-input-port port thunk))))

(define (with-output-to-file string thunk)
  (call-with-output-file string
    (lambda (port)
      (call-with-current-output-port port thunk))))

;----------------
; Flush the output buffers of all channel output ports.  This is done before
; forking the current process.

(define (force-channel-output-ports!)
  (for-each (lambda (port)
	      (if (port->channel port)
		  (force-output-if-open port)))
	    (periodically-flushed-ports)))

