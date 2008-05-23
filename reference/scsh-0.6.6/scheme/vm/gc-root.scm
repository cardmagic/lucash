; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; This manages external roots that can be registered and unregistered in a
; stack-like fashion.  It also provides the GC interface for the C FFI.

;----------------
; Stack of external roots implemented as a list.

(define *external-root-stack* null-address)

(define *external-root-stack-base* null-address)

; Format of a roots frame:
; 0: number of addresses in the frame, n
;    This may be negative---the frame then serves as a marker.
; 1: address of previous frame
; 2: location address of object 0
; 3: location address of object 1
; ...
; n+1: location address of object n-1

(define (frame-next frame)
  (fetch-address (address1+ frame)))

(define (frame-length frame)
  (fetch frame))

(define (frame-ref frame index)
  (fetch-address (address+ frame (cells->a-units (+ index 2)))))

; Fetch an address.

(define (fetch-address address)
  (integer->address (fetch address)))

;----------------
; Pushing and popping frames

; You call this with the number of roots to be registered and a frame
; which must contain space for n+2 pointers.

(define (s48-push-gc-roots! frame n)
  (store! frame n)
  (store! (address1+ frame)
	  (address->integer *external-root-stack*))
  (set! *external-root-stack* frame))

; We aren't allowed to pop past the current base.

(define (s48-pop-gc-roots!)
  (if (address= *external-root-stack* *external-root-stack-base*)
      #f
      (begin
	(set! *external-root-stack* (frame-next *external-root-stack*))
	#t)))

; We set the base before we jump to a C procedure from Scheme.  When it returns
; we check that everything that was pushed has been popped.  The caller saves
; the old base for us.

(define (s48-set-gc-roots-base!)
  (let ((old-base *external-root-stack-base*))
    (set! *external-root-stack-base* *external-root-stack*)
    (values old-base *external-root-stack*)))

(define (s48-release-gc-roots-base! old-base old-stack)
  (let ((okay? (address= *external-root-stack*
			 *external-root-stack-base*)))
    (set! *external-root-stack-base* old-base)
    (set! *external-root-stack* old-stack)
    okay?))

;----------------
; This is for permanent roots, such as external global variables.
; We keep these in a simple list.

(define *permanent-external-roots* null-address)

(define (s48-register-gc-root! loc-addr)
  (let ((frame (allocate-memory (cells->bytes 2))))
    (if (null-address? frame)
	(error "out of memory registering a global root"))
    (store! frame (address->integer *permanent-external-roots*))
    (store! (address1+ frame) (address->integer loc-addr))
    (set! *permanent-external-roots* frame)))

(define (permanent-root-pointer address)
  (fetch-address (address1+ address)))

(define (permanent-root-next address)
  (fetch-address address))

; This is exported and used to (attempt to) recover from user errors.

(define (s48-set-gc-roots-marker! marker)
  (set! *external-root-stack* marker))

;----------------
; Tracing
;
; We just walk down each list.

(add-gc-root!
  (lambda () 
    (trace-external-root-stack)
    (trace-permanent-external-roots)))

(define (trace-external-root-stack)
  (let loop ((frame *external-root-stack*))
    (if (not (null-address? frame))
	(let ((length (frame-length frame)))
	  (do ((i 0 (+ i 1)))
	      ((= i length))
	    (trace-cell (frame-ref frame i))) 
	  (loop (frame-next frame))))))

(define (trace-permanent-external-roots)
  (let loop ((frame *permanent-external-roots*))
    (if (not (null-address? frame))
	(begin
	  (trace-cell (permanent-root-pointer frame))
	  (loop (permanent-root-next frame))))))

; Trace the contents of a cell.

(define (trace-cell cell)
  (store! cell (s48-trace-value (fetch cell))))

;----------------
; The point to all this is, in part, to be able to allocate space while
; outside the VM.

(define (s48-allocate-stob type size)
  (make-b-vector type size (ensure-space size)))
