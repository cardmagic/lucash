;;; Copyright (c) 1993 by Olin Shivers.

(define (fd/port? x)
  (or (and (integer? x) (>= x 0))
      (output-port? x)
      (input-port? x)))


;;; Moves an i/o handle FD/PORT to fd TARGET.
;;; - If FD/PORT is a file descriptor, this is dup2(); close().
;;; - If FD/PORT is a port, this shifts the port's underlying file descriptor
;;;   to TARGET, as above, closing the old one. Port's revealed count is
;;;   set to 1.
;;; TARGET is evicted before the shift -- if there is a port allocated to
;;; file descriptor TARGET, it will be shifted to another file descriptor.

(define (move->fdes fd/port target)
  (let ((doit (lambda (fd)
		(if (not (= fd target))
		    (begin (evict-ports target) ; Evicts any ports at TARGET.
			   (%dup2 fd target))))))

    (cond ((integer? fd/port)
	   (doit fd/port)
	   target)

	  ((fdport? fd/port)
	   (sleazy-call/fdes fd/port doit)
	   (if (%move-fdport target fd/port 1)
	       (error "fdport shift failed."))
	   fd/port)

	  (else (error "Argument not fdport or file descriptor" fd/port)))))


(define (input-source? fd/port)
  (check-arg fd/port? fd/port input-source?)
  (or (input-port? fd/port)
      (and (integer? fd/port)
	   (let ((access (bitwise-and open/access-mask (fdes-status fd/port))))
	     (or (= access open/read)
		 (= access open/read+write))))))

(define (output-source? fd/port)
  (check-arg fd/port? fd/port output-source?)
  (or (output-port? fd/port)
      (and (integer? fd/port)
	   (let ((access (bitwise-and open/access-mask (fdes-status fd/port))))
	     (or (= access open/write)
		 (= access open/read+write))))))


;;; If FD/PORT is a file descriptor, returns a file descriptor.
;;; If FD/PORT is a port, returns a port.

(define (dup fd/port . maybe-target)
  (check-arg fd/port? fd/port dup)
  (apply (cond ((integer? fd/port) dup->fdes)
	       ((input-port?  fd/port) dup->inport)
	       ((output-port? fd/port) dup->outport))
	 fd/port maybe-target))

(define (dup->fdes fd/port . maybe-target)
  (check-arg fd/port? fd/port dup->fdes)
  (if (pair? maybe-target)
      (let ((target (car maybe-target)))
	(close-fdes target)	; Thus evicting any port there.
	(sleazy-call/fdes fd/port (lambda (fd) (%dup2 fd target))))
      (sleazy-call/fdes fd/port %dup)))

(define (dup->inport fd/port . maybe-target)
  (apply really-dup->port make-input-fdport fd/port maybe-target))

(define (dup->outport fd/port . maybe-target)
  (apply really-dup->port make-output-fdport fd/port maybe-target))

(define (really-dup->port port-maker fd/port . maybe-target)
  (let ((fd (apply dup->fdes fd/port maybe-target)))
    (port-maker fd (if (null? maybe-target) 0 1))))


;;; Not exported.
(define (shell-open path flags fdes)
  (move->fdes (open-fdes (stringify path) flags #o666) fdes))

(define open/create+trunc
  (bitwise-ior open/write (bitwise-ior open/create open/truncate)))

(define open/write+append+create
  (bitwise-ior open/write 
	       (bitwise-ior  open/append open/create)))
