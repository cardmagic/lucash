; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; The value of $NOTE-FILE-PACKAGE is called whenever a file is loaded into
; a package.  env/debug.scm uses this to associate packages with files so
; that code stuffed to the REPL will be eval'ed in the correct package.
;
; Is there any point in having this be a fluid?

(define $note-file-package
  (make-fluid (lambda (filename package)
		(values))))

(define (read-forms pathname package)
  (let* ((filename (namestring pathname #f *scheme-file-type*))
	 (truename (translate filename))
	 (port (open-input-file truename)))
    (dynamic-wind
     (lambda ()
       (if (not port)
	   (error "attempt to throw back into a file read"))) ; message needs work
     (lambda ()
       ((fluid $note-file-package) filename package)
       (let ((o-port (current-noise-port)))
	 (display truename o-port)
	 (force-output o-port)
	 (read-forms-from-port port)))
     (lambda ()
       (close-input-port port)
       (set! port #f)))))

(define (read-forms-from-port port)	    
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form)
	  (reverse forms)
	  (loop (cons form forms))))))


