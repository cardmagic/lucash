;;; (DEFINE-FOREIGN ...) forms are expanded by Cig into Scheme stubs.
;;; These stubs reference some support procedures to rep-convert the
;;; standard reps (e.g., string). This structure provides these support 
;;; procedures.
;;;
;;; We export three kinds of things:
;;; - Type predicates that aren't in the R4RS env (e.g., FIXNUM?).
;;; - Carrier makers for making boxes to return things in.
;;; - Scheme-side rep-converters for return values.

(define-structure cig-aux
  (export cstring-null?
	  C->scheme-string
	  C->scheme-string-w/len
	  C->scheme-string-w/len-no-free
	  C-string-vec->Scheme&free
	  C-string-vec->Scheme ; Bogus, because clients not reentrant.
	  string-carrier->string
	  string-carrier->string-no-free
	  fixnum?
	  make-string-carrier
	  make-alien
	  alien?
	  )
  (open scheme code-vectors define-foreign-syntax)

  (begin
    (define min-fixnum (- (expt 2 29)))
    (define max-fixnum (- (expt 2 29) 1))
    (define (fixnum? x) (and (integer? x) (<= min-fixnum x max-fixnum)))

    ;; Internal utility.
    (define (mapv! f v)
      (let ((len (vector-length v)))
	(do ((i 0 (+ i 1)))
	    ((= i len) v)
	  (vector-set! v i (f (vector-ref v i))))))

    ;; Make a carrier for returning strings. 
    ;; It holds a raw C string and a fixnum giving the length of the string.
    (define (make-string-carrier) (cons (make-alien) 0))

    (define (make-alien) (make-code-vector 4 0))
    (define (alien? x) (and (code-vector? x) (= 4 (code-vector-length x)))) ; BOGUS


;;; C/Scheme string and vector conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generally speaking, in the following routines, 
;;; a NULL C string param causes a function to return #f.
(foreign-init-name "cig") 

(define-foreign %cstring-length-or-false
  (strlen_or_false ((C "const char * ~a") cstr))
  desc)

(define-foreign cstring-null?
  (cstring_nullp ((C "const char * ~a") cstr))
  bool)

(define-foreign %copy-c-string&free
  (c2scheme_strcpy_free (string-desc sstr) ((C char*) cstr))
  bool)

(define-foreign %copy-c-string
  (c2scheme_strcpy (string-desc sstr) ((C char*) cstr))
  bool)

(define (C->scheme-string cstr)
  (cond ((%cstring-length-or-false cstr)
	 => (lambda (strlen)
	      (let ((str (make-string strlen)))
		(%copy-c-string&free str cstr)
		str)))
	(else #f)))

(define (C->scheme-string-w/len cstr len)
  (and (integer? len)
       (let ((str (make-string len)))
	 (%copy-c-string&free str cstr)
	 str)))

(define (C->scheme-string-w/len-no-free cstr len)
  (and (integer? len)
       (let ((str (make-string len)))
	 (%copy-c-string str cstr)
	 str)))

(define (string-carrier->string carrier)
  (C->scheme-string-w/len (car carrier) (cdr carrier)))

(define (string-carrier->string-no-free carrier)
  (C->scheme-string-w/len-no-free (car carrier) (cdr carrier)))

;;; Return the length of a null-terminated C word vector. 
;;; Does not count the null word as part of the length.
;;; If vector is NULL, returns #f.

(define-foreign %c-veclen-or-false
  (c_veclen ((C long*) c-vec))
  desc) ; integer or #f if arg is NULL.

;;; CVEC is a C vector of char* strings, length VECLEN.
;;; This procedure converts a C vector of strings into a Scheme vector of 
;;; strings. The C vector and its strings are all assumed to come from
;;; the malloc heap; they are returned to the heap when the rep-conversion
;;; is done.
;;;
;;; Hack: if VECLEN is #f, CVEC is assumed to be NULL terminated, and
;;; its length is calculated thusly.

(define (C-string-vec->Scheme&free cvec veclen)
  (let ((vec (make-vector (or veclen (%c-veclen-or-false cvec) 0))))
    (mapv! (lambda (ignore) (make-string-carrier)) vec)
    (%set-string-vector-carriers! vec cvec)
    (C-free cvec)
    (mapv! string-carrier->string vec)))

(define (C-string-vec->Scheme cvec veclen) ; No free.
  (let ((vec (make-vector (or veclen (%c-veclen-or-false cvec) 0))))
    (mapv! (lambda (ignore) (make-string-carrier)) vec)
    (%set-string-vector-carriers! vec cvec)
    (mapv! string-carrier->string-no-free vec)))


(define-foreign C-free (free ((C void*) ptr)) no-declare ; for SunOS 4.x 
  ignore)

(define-foreign %set-string-vector-carriers!
  (set_strvec_carriers (vector-desc svec) ((C char**) cvec))
  ignore)

)) ; egakcap





