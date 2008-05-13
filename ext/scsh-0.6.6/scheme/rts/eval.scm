; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; This file contains things that tie together the compiler and the
; run-time system.

; EVAL

(define (eval form package)
  (compile-and-run (list form) package #f #f))

; LOAD-INTO - load file into package.

(define (load-into filename package)
  (really-load-into filename package #f))  

; Evaluate forms as if they came from the given file.

(define (eval-from-file forms package filename)
  (if filename
      ((fluid $note-file-package)
        filename package))
  (compile-and-run forms package filename #t))

; LOAD

(define (load filename . package-option)
  (let ((package (if (null? package-option)
		     (interaction-environment)
		     (car package-option))))
    (really-load-into filename package #t)))

(define (load-port port . package-option)
  (let ((package (if (null? package-option)
                     (interaction-environment)
                     (car package-option)))
        (forms (read-forms-from-port port)))
    (compile-and-run forms package #f #t)))

;----------------

(define (really-load-into filename package note-undefined?)  
  (force-output (current-output-port))	; just to make the output nice
  (let ((forms (read-forms filename package)))
    (newline (current-noise-port))	; READ-FORMS prints the filename
    (compile-and-run forms
		     package
		     filename
		     note-undefined?)))

(define (compile-and-run forms package maybe-filename note-undefined?)
  (let* ((env (if maybe-filename
		  (bind-source-file-name maybe-filename
					 (package->environment package))
		  (package->environment package)))
	 (template (compile-forms (map (lambda (form)
					 (delay (expand-scanned-form form env)))
				       (scan-forms forms env))
				  maybe-filename)))
    (link! template package note-undefined?)
    (invoke-closure
      (make-closure template
		    (package-uid package)))))


