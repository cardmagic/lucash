;;; Pseudo terminals
;;; Copyright (c) 1995 by Olin Shivers.

;;; (fork-pty-session thunk)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fork the process with stdio (fd's 0, 1, & 2 and also the current i/o ports)
;;; bound to a tty device. In the parent process, returns four values:
;;;     [process pty-inport pty-outport ttyname]
;;; - PROCESS is a process object for the child.
;;; - PTY-{IN,OUT}PORT are input and output ports open on the controlling pty
;;;   device. PTY-OUTPORT is unbuffered.
;;; - TTYNAME is the name of the child's tty, e.g. "/dev/ttyk4".
;;; 
;;; The subprocess is placed in its own session, and the tty device
;;; becomes the control tty for the new session/process-group/process. 
;;; The child runs with stio hooked up to the tty; the (error-output-port)
;;; port is unbuffered.

(define (fork-pty-session thunk)
  (receive (pty-in ttyname) (open-pty)
    (let* ((process (fork (lambda ()
			    (close-input-port pty-in)
			    (become-session-leader)
			    (let ((tty (open-control-tty ttyname)))
			      (move->fdes   tty 0)
			      (dup->outport tty 1)
			      (dup->outport tty 2))
;			      (set-port-buffering (dup->outport tty 2)
;						  bufpol/none))
			    (with-stdio-ports* thunk))))
	   (pty-out (dup->outport pty-in)))

;      (set-port-buffering pty-out bufpol/none)
      (values process pty-in pty-out ttyname))))

;;; (open-pty)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns two values: [pty-inport ttyname]
;;; PTY-PORT is a port open on the pty.
;;; TTYNAME is the name of the tty, e.g., "/dev/ttyk4"
;;;
;;; Scheme doesn't allow bidirectional ports, so the returned port
;;; is an input port -- however, the underlying file descriptor is
;;; opened read+write, and you can use DUP->OUTPORT to map it to
;;; corresponding output ports.

(define (open-pty)
  (let ((next-pty (make-pty-generator)))
    (let loop ()
      (cond ((next-pty) =>
	     (lambda (pty-name)
	       (cond ((with-errno-handler ((errno packet) (else #f))
		        (open-file pty-name open/read+write)) =>
		      (lambda (pty) ; Score!
			(values pty (pty-name->tty-name pty-name))))

		     (else (loop))))) ; Open failed; try another pty.

	    (else (error "open-pty: could not open new pty"))))))

;;; The following code may in fact be system dependent.
;;; If so, we'll move it out to the architecture specific directories.

;;; Map between corresponding pty and tty filenames.

(define (pty/tty-name-mapper char)
  (lambda (name)
    (let ((ans (string-copy name)))
      (string-set! ans 5 char)		; Change X in "/dev/Xtyzz" to CHAR.
      ans)))
      
(define pty-name->tty-name (pty/tty-name-mapper #\t)) ;/dev/ttyk3 -> /dev/ptyk3
(define tty-name->pty-name (pty/tty-name-mapper #\p)) ;/dev/ptyk3 -> /dev/ttyk3


;;; Generator for the set of possible pty names.

(define (make-pty-generator)
  (let* ((pattern (string-copy"/dev/ptyLN")) ; L=letter N=number
	 (l-pos 8)
	 (n-pos 9)

;	 (letters "pqrstuvwxyzPQRST")	; From telnetd source in BSD4.4.
;	 (numbers "0123456789abcdef")
	 (letters "pq")	; From telnetd source in BSD4.4.
	 (numbers "0123456789abcdef")
	 (num-letters (string-length letters))
	 (num-numbers (string-length numbers))

	 (l num-letters)	; Generator's state vars. The value
	 (n 0))			; of the last elt that was generated.
    				; (We count backwards to (0,0); n fastest.)
    (lambda ()
      (call-with-current-continuation
        (lambda (abort)
	  (if (zero? n)
	      (if (zero? l) (abort #f)	; No more.
		  (begin (set! l (- l 1))
			 (set! n (- num-numbers 1))
			 (string-set! pattern l-pos (string-ref letters l))))
	      (set! n (- n 1)))
	  (string-set! pattern n-pos (string-ref numbers n))
	  (string-copy pattern))))))
