; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Continuations

(define (continuation-cont                c) (continuation-ref c 0))
(define (real-continuation-pc             c) (continuation-ref c 1))
(define (real-continuation-template       c) (continuation-ref c 2))
(define (continuation-env                 c) (continuation-ref c 3))
(define (exception-continuation-pc        c) (continuation-ref c 4))
(define (exception-continuation-template  c) (continuation-ref c 5))
(define (exception-continuation-exception c) (continuation-ref c 6))

; Exception continuations contain the state of the VM when an exception occured.

(define (exception-continuation? thing)
  (and (continuation? thing)
       (= 0 (real-continuation-pc thing))
       (= (enum op return-from-exception)
	  (code-vector-ref (template-code (real-continuation-template thing))
			   0))))

(define (continuation-pc c)
  (if (exception-continuation? c)
      (exception-continuation-pc c)
      (real-continuation-pc c)))

(define (continuation-template c)
  (if (exception-continuation? c)
      (exception-continuation-template c)
      (real-continuation-template c)))

; Accessing the saved operand stack.

(define normal-continuation-overhead 4)

(define exception-continuation-overhead
  (+ normal-continuation-overhead 4))

(define (continuation-arg c i)
  (continuation-ref c (+ (if (exception-continuation? c)
			     exception-continuation-overhead
			     normal-continuation-overhead)
			 i)))

(define (continuation-arg-count c)
  (- (continuation-length c)
     (if (exception-continuation? c)
	 exception-continuation-overhead
	 normal-continuation-overhead)))

(define-simple-type :continuation (:value) continuation?)

(define-method &disclose ((obj :continuation))
  (if (exception-continuation? obj)
      (list 'exception-continuation
	    `(pc ,(exception-continuation-pc obj))
	    (template-info (exception-continuation-template obj)))
      (list 'continuation
	    `(pc ,(continuation-pc obj))
	    (template-info (continuation-template obj)))))

; If (continuation-cont A) = B, then ignore B if the following are all true:
;   1. (continuation-template B) = (continuation-template A)
;   2. (continuation-pc B) > (continuation-pc A)
;   3. (continuation-env B) = (continuation-env A)
;                             or some parent of (continuation-env A)
;
; I don't think this is foolproof, but I have so far been unable to
; contrive a situation in which it fails.  I think a double recursion of a 
; procedure of no arguments is required, at the very least.

(define (continuation-parent a)
  (let ((b (continuation-cont a)))
    (if (and (continuation? b)
	     (eq? (continuation-template a) (continuation-template b))
	     (< (continuation-pc a) (continuation-pc b))
	     (let loop ((env (continuation-env a)))
	       (or (eq? env (continuation-env b))
		   (and (vector? env)
			(loop (vector-ref env 0))))))
	(continuation-parent b)
	b)))
