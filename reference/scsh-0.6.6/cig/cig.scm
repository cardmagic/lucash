;;; This file defines the cig foreign function interface for Scheme 48.
;;; The current version is Cig 3.0.
;;; This file contains the following Scheme 48 modules:
;;; - cig-processor
;;;   The code for translating DEFINE-FOREIGN forms into C stubs.
;;; - cig-standalone
;;;   The S48 top-level for translating stdin->stdout.
;;; - define-foreign-syntax-support
;;;   This package must be opened in the FOR-SYNTAX package,
;;;   so that the DEFINE-FOREIGN macro-expander code can use it's procedures.
;;; - define-foreign-syntax
;;;   This package must be opened by cig's clients, to access the
;;;   DEFINE-FOREIGN and FOREIGN-INCLUDE macros.
;;;
;;; Copyright (c) 1994 by Olin Shivers.

(define-structures ((cig-processor (export process-define-foreign-file
					   process-define-foreign-stream))
		    (cig-standalone (export cig-standalone-toplevel))

		    ;; This must be opened in the FOR-SYNTAX package.
		    (define-foreign-syntax-support
		      (export define-foreign-expander)))

  (open scheme formats structure-refs
	destructuring receiving
	code-vectors) ; for making alien containers.
  (access signals) ; for ERROR
  (begin
    (define error (structure-ref signals error))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The general syntax of define-foreign is:
;;; (define-foreign scheme-name (c-name arg1 ... argn) [no-declare]
;;;   ret1
;;;    .
;;;   retn)
;;; 
;;; This defines a Scheme procedure, <scheme-name>. It takes the arguments
;;; arg1 ... argn, type-checks them, and then passes them to a C stub,
;;; df_<c-name>. If the Scheme procedure is to return multiple values, the C
;;; stub also gets a return vector passed to return the extra values. The C
;;; stub rep-converts the Scheme data as specified by the <arg>i declarations,
;;; and then calls the C procedure <c-name>.  The C procedure is expected to
;;; return its first value (<ret1>) as its real value. The other return values
;;; are returned by assigning targets passed by-reference to <c-name> by the
;;; stub. These return parameters are passed after the argument parameters.
;;; When <c-name> returns, the C stub df_<c-name> rep-converts the C data,
;;; stuffs extra return values into the Scheme answer vector if there are any,
;;; and returns to the Scheme routine. The Scheme routine completes the
;;; rep-conversion specified by the <ret>i declarations, and return the
;;; values.
;;; 
;;; An ARGi spec has the form:
;;;     (rep [var])
;;; where REP gives the representation of the value being passed (see
;;; below), and VAR is the name of the Scheme procedure's parameter (for
;;; documentation purposes, mostly).
;;;
;;; The optional symbol NO-DECLARE means "Do not place an extern declaration
;;; of the C routine in the body of the stub." This is necessary for the
;;; occasional strange ANSI C declaration that cig is incapable of generating
;;; (the only case I know of where the C procedure uses varargs, so the C
;;; declaration needs a ..., e.g.,
;;; 	extern int open(const char *, int flags, ...);
;;; In this case, just use NO-DECLARE, and insert your own a declaration of open()
;;; outside the stub with a
;;; 	(foreign-source "extern int open(const char *, int flags, ...);")
;;; Ugly, yes.)
;;; 
;;; The rep-conversion specs are pretty hairy and baroque. I kept throwing
;;; in machinery until I was able to handle all the Unix syscalls, so that
;;; is what drove the complexity level. See syscalls.scm for examples.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The fields of a rep record for argument reps:
;;; Scheme-pred: 
;;;    A Scheme predicate for type-testing args. #f means no check.
;;; C-decl: 
;;;    A C declaration for the argument in its C representation --
;;;    the type of the value actually passed to or returned from the foreign
;;;    function. This is a format string; the ~a is where the C variable goes.
;;;    (format #f c-decl "") is used to compute a pure type -- e.g., for
;;;    casts.
;;; C-cvtr:
;;;    The Scheme->C rep-converter; a string. Applied as a C 
;;;    function/macro in the stub. The empty string means the null
;;;    rep-conversion.
;;; Post-C:
;;;     Optional post-call processing in the C stub; a string like C-cvtr.
;;;     If not #f, this form will be applied in the C stub to the C argument
;;;     value *after* the C call returns. It is mostly used to free a
;;;     block of storage that was malloc'd by the rep converter on the
;;;     way in.

(define (argrep:c-decl i)      (vector-ref i 0))
(define (argrep:scheme-pred i) (vector-ref i 1))
(define (argrep:c-cvtr i) 	 (vector-ref i 2))
(define (argrep:post-C i) 	 (vector-ref i 3))


;;; The fields of a rep record for return reps:
;;; C-decl:
;;;    As above.
;;; immediate?:
;;;    If the return value is to be boxed into a carrier passed in from
;;;    Scheme, then this is #f. If this value is a true value, then the
;;;    C value is to be rep-converted into an immediate Scheme value.
;;;    In this case, the immediate? field is a string, naming the C
;;;    function/macro used to do the rep-conversion.
;;; C-boxcvtr:
;;;    If immediate? is false, then this value specifies the C code
;;;    for rep-converting the return value into the Scheme carrier.
;;;    It is a procedure, which is called on two string arguments:
;;;    a C variable bound to the carrier, and a C variable bound to
;;;    the C return value. The procedure returns a string which is a
;;;    C statement for doing the rep-conversion. To pass a raw C value
;;;    back, for instance, you would use the following box converter:
;;;        (lambda (carrier c-val) (string-append carrier "=" c-val ";"))
;;; make-carrier:
;;;    A procedure that when called returns a carrier. This field is only
;;;    used if immediate? is #f. This field is a Scheme expression.
;;; S-cvtr
;;;    This is a Scheme form that is applied to the rep-converted value passed
;;;    back from the C stub. Its value is the actual return value returned to
;;;    Scheme. #f means just pass a single value back as-is. This is mostly
;;;    used for string hacking. This field is a Scheme expression.

(define (retrep:c-decl i)		(vector-ref i 0))
(define (retrep:immediate i)		(vector-ref i 1))
(define (retrep:C-boxcvtr i)		(vector-ref i 2))
(define (retrep:make-carrier i)		(vector-ref i 3))
(define (retrep:s-cvtr i)		(vector-ref i 4))

;;; Works for both argrep-info and retrep-info nodes.
(define (rep:c-decl i) (vector-ref i 0))

;;; The Scheme-pred field in this table is a symbol that is syntactically
;;; closed in the macro expander's environment, so the user won't lose
;;; if he should accidentally bind INTEGER? to something unusual, and
;;; then try a DEFINE-FOREIGN.

;;; WARNING: the xxx_t are not yet adapted to the new FFI, only time_t, 
;;; long and int are entered via enter/extract_integer, which converts to a 
;;; bignum if appropriate.
;;; Since this requires runtime-checks it is slower than the _integer's
;;; You have to check, where more than 29 bits is required

(define *simple-argrep-alist* '(

 (char   #("char ~a"	char?	"s48_extract_char"  #f))
 (bool   #("int ~a"	#f	"EXTRACT_BOOLEAN"  #f))

 (integer  #("int ~a" 		  integer?   "s48_extract_integer"   #f))
 (short_u  #("unsigned short ~a"  integer?   "s48_extract_fixnum"	#f))
 (size_t   #("size_t ~a"          integer?   "s48_extract_fixnum"	#f))
 (mode_t   #("mode_t ~a" 	  integer?   "s48_extract_fixnum"	#f))
 (gid_t    #("gid_t  ~a" 	  integer?   "s48_extract_fixnum"	#f))
 (uid_t    #("uid_t ~a" 	  integer?   "s48_extract_fixnum"	#f))
 (off_t    #("off_t ~a" 	  integer?   "s48_extract_fixnum"	#f))
 (pid_t    #("pid_t ~a" 	  integer?   "s48_extract_fixnum"	#f))
 (uint_t   #("unsigned int ~a"	  integer?   "s48_extract_integer"	#f))
 (long     #("long ~a"		  integer?   "s48_extract_integer"	#f))
 (fixnum   #("int ~a"	  	  fixnum?    "s48_extract_fixnum"	#f))

 ;; extensions by JMG for the new FFI
 (time_t  #("time_t ~a" 	  integer?   "s48_extract_integer"	#f))
 (clock_t #("clock_t ~a"	  integer?   "s48_extract_fixnum"	#f))
 (ssize_t #("ssize_t ~a"	  integer?   "s48_extract_fixnum"	#f))

 (desc		 #("s48_value ~a"	#f	 "" #f))
 (string-desc	 #("s48_value ~a"	string?	 "" #f))
 (char-desc	 #("s48_value ~a"	char?	 "" #f))
 (integer-desc	 #("s48_value ~a"	integer? "" #f))
 (vector-desc	 #("s48_value ~a"	vector?	 "" #f))
 (pair-desc	 #("s48_value ~a"	pair?	 "" #f))

 (string 	#("const char *~a" 	string? "s48_extract_string" #f))

 (var-string 	#("char *~a" 		string? "s48_extract_string" #f))

 (string-copy 	#("char *~a" 		string? "scheme2c_strcpy" #f))))

;;; Emit C code to copy a C string into its carrier.
(define (str-and-len->carrier carrier str)
  (format #f
	  "SetAlienVal(S48_CAR(~a),(long) ~a); S48_SET_CDR(~a,strlen_or_false(~a));"
	  carrier str carrier str))

;;; Carrier and assignment-generator for alien values:
(define (simple-assign carrier val)
  (format #f "SetAlienVal(~a,(long) ~a);" carrier val)) ;;JMG: untested

;;; Note: When MAKE-CARRIER and S-CVTR fields are taken from this table,
;;; they are symbols that are syntactically closed in the macro expander's
;;; environment by using the expander's rename procedure. This ensures that
;;; even if the user accidentally binds his own MAKE-ALIEN identifier,
;;; he won't clobber the Scheme stub's use of this MAKE-ALIEN procedure.

(define *simple-retrep-alist* `(

 ;; All the immediate ones (we are sleazing on ints for now).
 (char   #("char ~a" "s48_enter_char" #f #f #f))
 (bool   #("int ~a"  "ENTER_BOOLEAN" #f #f #f)) ;; JMG c and bool: 
 ; s48 knows nothing about this I think

 (integer  #("int ~a"	         "s48_enter_integer" #f #f #f))
 (fixnum   #("int ~a"	         "s48_enter_fixnum" #f #f #f))
 (short_u  #("unsigned short ~a" "s48_enter_fixnum" #f #f #f))
 (size_t   #("size_t ~a"	 "s48_enter_fixnum" #f #f #f))
 (mode_t   #("mode_t ~a"	 "s48_enter_fixnum" #f #f #f))
 (gid_t    #("gid_t  ~a"	 "s48_enter_fixnum" #f #f #f))
 (uid_t    #("uid_t ~a"		 "s48_enter_fixnum" #f #f #f))
 (off_t    #("off_t ~a"		 "s48_enter_fixnum" #f #f #f))
 (pid_t    #("pid_t ~a"		 "s48_enter_fixnum" #f #f #f))
 (uint_t   #("unsigned int ~a"	 "s48_enter_fixnum" #f #f #f))
 (long     #("long ~a"		 "s48_enter_integer" #f #f #f))

 ; Extension by JMG with the new FFI
 (time_t  #("time_t ~a"		 "s48_enter_integer" #f #f #f))
 (clock_t #("clock_t ~a"	 "s48_enter_fixnum" #f #f #f))
 (ssize_t #("ssize_t ~a"	 "s48_enter_fixnum" #f #f #f))


 (desc		 #("s48_value ~a" "" #f #f #f))
 (string-desc	 #("s48_value ~a" "" #f #f #f))
 (char-desc	 #("s48_value ~a" "" #f #f #f))
 (integer-desc	 #("s48_value ~a" "" #f #f #f))
 (vector-desc	 #("s48_value ~a" "" #f #f #f))
 (pair-desc	 #("s48_value ~a" "" #f #f #f))

 (string 	#("const char *~a" #f ,str-and-len->carrier make-string-carrier
				   string-carrier->string))

 (var-string 	#("char *~a" #f ,str-and-len->carrier make-string-carrier
			     string-carrier->string))

 (string-length	#("char *~a" "strlen_or_false" #f #f #f))

 (static-string	#("char *~a"  #f ,str-and-len->carrier make-string-carrier
			      string-carrier->string-no-free))))

;;; String reps:
;;; -----------
;;; - STRING-COPY
;;;   Parameter only. The C routine is given a private, malloc'd C string.
;;;   The string is not freed when the routine returns.
;;;
;;; - STRING
;;;   Parameter: The C routine is given a C string that it should not alter
;;;   or retain beyond the end of the routine. Right now, the Scheme string
;;;   is copied to a malloc'd C temporary, which is freed after the routine
;;;   returns. Later, we'll just pass a pointer into the actual Scheme
;;;   string, as soon as Richard fixes the S48 string reps.
;;;   Ret value: The C string is from malloc'd storage. Convert it to a
;;;     Scheme string and free the C string.
;;;
;;; - STRING-LENGTH
;;;   Return-value only. Return the length of the C string, as a fixnum.
;;;
;;; - STATIC-STRING
;;;   Return-value only. The C string is not freed after converting it to
;;;   to a Scheme string.
;;;
;;; - VAR-STRING
;;;   Same  as STRING, but C type is declared char* instead of const char*.
;;;   Used to keep some broken system call include files happy.

;;; Parameter reps:
;;; - A simple rep is simply the name of a record in the rep table.
;;;   e.g., integer, string
;;; - (REP scheme-pred c-decl to-c [free?])
;;;   A detailed spec, as outlined above. SCHEME-PRED is a procedure or #f.
;;;   C-DECL is a format string (or a symbol). TO-C is a format string
;;;   (or a symbol).
;;; - (C type)
;;;   The argument is a C value, passed with no type-checking
;;;   or rep-conversion. TYPE is a format string (or a symbol).

;;; A return-value rep is:
;;; - A simple rep, as above.
;;; - (MULTI-REP rep1 ... repn)
;;;   The single value returned from the C function is rep-converted
;;;   n ways, each resulting in a distinct return value from Scheme.
;;; - (TO-SCHEME rep c->scheme)
;;;   Identical to REP, but use the single C->SCHEME form for the return
;;;   rep-conversion in the C stub. There is no POST-SCHEME processing. This
;;;   allows you to use a special rep-converter on the C side, but otherwise
;;;   use all the properties of some standard rep. C->SCHEME is a string (or
;;;   symbol).
;;; - (C type)
;;;   Returns a raw C type. No rep-conversion. TYPE is a C type, represented
;;;   as a string (or a symbol).

;;; C Short-hand:
;;; Things that go in the C code are usually specified as strings,
;;; since C is case-sensitive, and Scheme symbols are not. However,
;;; as a convenient short-hand, symbols may also be used -- they
;;; are mapped to strings by lower-casing their print names. This
;;; applies to the TO-C part of (REP ...) and the C->SCHEME part of 
;;; TO-SCHEME.
;;;
;;; Furthermore, C declarations (the TYPE part of (C ...) and the C-DECL part
;;; of (REP ...)) can be either a format string (e.g., "char ~a[]"), or a
;;; symbol (double). A symbol is converted to a string by lower-casing it, and
;;; appending " ~a", so the symbol double is just convenient short-hand for
;;; the C declaration "double ~a".
;;;
;;; Examples: (rep integer? int "EXTRACT_FIXNUM")
;;;           (C char*)
;;;           (C "int ~a[10]")
;;;           (to-scheme integer "HackInt")
;;;
;;; These shorthand forms are not permitted in the actual rep tables; 
;;; only in DEFINE-FOREIGN forms.

;;; Note: the RENAME procedure is for use by the Scheme-stub macro expander
;;; when taking SCHEME-PRED fields from the simple-rep internal table. This
;;; way, the user's bindings of variables won't interfere with the functioning
;;; of the simple reps. When Cig's C-stub generator calls this procedure, it
;;; should just pass the identity procedure for the RENAME argument.

(define (parameter-rep->info rep rename)
  (let* ((hack (lambda (x)
		 (if (symbol? x) (string-append (symbol->string x) " ~a")
		     x)))
	 (do-rep (lambda (scheme-pred C-decl C-cvtr . maybe-post-C)
		   (vector (hack C-decl) scheme-pred (stringify C-cvtr)
			   (and (pair? maybe-post-C) (car maybe-post-C)))))
	 (you-lose (lambda () (error "Unknown parameter rep" rep))))

    (cond ((symbol? rep)
	   (cond ((assq rep *simple-argrep-alist*) =>
		  (lambda (entry)
		    (let* ((info (copy-vector (cadr entry)))
			   (scheme-pred (argrep:scheme-pred info)))
		      (vector-set! info 1 (and scheme-pred (rename scheme-pred)))
		      info)))

		 (else (you-lose))))
	  
	  ((pair? rep)
	   (case (car rep)
	     ((rep) (apply do-rep (cdr rep)))
	     ((C) (let* ((c-decl (hack (cadr rep)))
			 (c-type (format #f c-decl "")))
		    (do-rep (rename 'alien?) c-decl 
			    (format #f "(~a)AlienVal" c-type)
			    #f)))
	     (else (you-lose))))
	  (else (you-lose)))))

(define (copy-vector v)
  (let* ((vlen (vector-length v))
	 (v-new (make-vector vlen)))
    (do ((i (- vlen 1) (- i 1)))
	((< i 0) v-new)
      (vector-set! v-new i (vector-ref v i)))))

(define (stringify x)
  (if (symbol? x)
      (list->string (map char-downcase (string->list (symbol->string x))))
      x))

;;; Fields are as follows:
;;; c-decl: 0,  immediate: 1, C-boxcvtr: 2,  make-carrier: 3,  s-cvtr: 4

;;; Return a list of reps (because of MULTI-REP).
;;; The RENAME arg is for the Scheme-side macro expander, so that
;;; the make-carrier and s-cvtr fields can be syntactically closed
;;; in the expander's environment. The C-stub generator should just
;;; pass an identity procedure for RENAME.

(define (return-rep->info rep rename)
  (let* ((hack (lambda (x)
		 (if (symbol? x)
		     (string-append (symbol->string x) " ~a")
		     x)))
	 (do-rep (lambda (c-decl . to-scheme)
		   (list (vector (hack c-decl) (list to-scheme) '() #f))))
	 (you-lose (lambda () (error "Unknown return rep" rep)))
    
	 (infos (cond ((symbol? rep)
		       (cond ((assq rep *simple-retrep-alist*) =>
			      (lambda (entry)
				;; Apply RENAME to make-carrier and s-cvtr.
				(let* ((info (copy-vector (cadr entry)))
				       (make-carrier (retrep:make-carrier info))
				       (s-cvtr (retrep:s-cvtr info)))
				  (vector-set! info 3
					       (and make-carrier
						    (rename make-carrier)))
				  (vector-set! info 4
					       (and s-cvtr (rename s-cvtr)))
				  (list info))))
			     (else (you-lose))))
	  
		      ((pair? rep)
		       (case (car rep)
			 ((rep)
			  (let ((v (apply vector rep)))
			    (vector-set! v 0 (hack (vector-ref v 0)))
			    (list v)))
			 ((to-scheme)	; (to-scheme rep converter)
			  (let* ((v (car (return-rep->info (cadr rep) rename)))
				 (v (copy-vector v)))
			    (vector-set! v 1 (stringify (caddr rep)))
			    (vector-set! v 2 '#f)
			    (vector-set! v 3 '#f)
			    (vector-set! v 4 '#f)
			    (list v)))
			 ((C) (list (vector (hack (cadr rep)) #f
					    simple-assign (rename 'make-alien)
					    #f)))
			 ((multi-rep)
			  (apply append (map (lambda (rep)
					       (return-rep->info rep rename))
					     (cdr rep))))
			 (else (you-lose))))
		     (else (you-lose)))))

    infos))

;;; Return a type string for IGNORE, or a list of lists of info vectors for
;;; the standard case.

(define (parse-return-reps reps rename)
  (cond ((or (not (pair? reps))
	     (not (list? reps)))
	 (error "Bad return rep list" reps))
	
	;; (IGNORE c-type) or IGNORE
	((and (null? (cdr reps))
	      (let ((rep (car reps)))
		(or (eq? rep 'ignore)
		    (and (pair? rep)
			 (eq? (car rep) 'ignore)))))
	 (let ((rep (car reps)))
	   (if (pair? rep) (cadr rep) "void ~a")))
	
	(else (map (lambda (rep) (return-rep->info rep rename)) reps))))

(define (insert-commas lis)
  (if (pair? lis)
      (cdr (let rec ((lis lis))
	     (if (pair? lis)
		 (cons ", " (cons (car lis) (rec (cdr lis))))
		 '())))
      '("")))

(define (elts->comma-string lis)
  (apply string-append (insert-commas lis)))

(define (info->type i . maybe-outer-type)
  (let ((outer-type (if (null? maybe-outer-type) "" (car maybe-outer-type))))
    (format #f (rep:c-decl i) outer-type)))

(define (info->var-decl i var)
  (format #f "~%    ~a = 0;" ; statement-ize decl.
	  (format #f (rep:c-decl i) var))) ; decl-ize var.

(define (make-gensym prefix i)
  (lambda (x)
    (set! i (+ i 1))
    (string-append prefix (number->string i))))

;;; This returns a list mapping each of the Scheme stub's args to
;;; it's corresponding name in the C stub (e.g., ("arg[2]" "arg[1]" "arg[0]")).
;;; If MV-RETURN? is true, we reserve arg[0] for the mv-return Scheme vec.
(define (make-stub-args nargs mv-return?)
  (do ((i (if mv-return? 1 0) (+ i 1))
       (nargs nargs (- nargs 1))
       (ans '() (cons (format #f "args[~d]" i) ans)))
      ((zero? nargs) ans)))

(define (filter lis)
  (if (pair? lis)
      (let* ((head (car lis))
	     (tail (cdr lis))
	     (new-tail (filter  tail)))
	(if head (if (eq? tail new-tail) lis (cons head new-tail))
	    new-tail))
      '()))

(define nl (string #\newline))
(define (separate-line stmt) (string-append "    " stmt ";" nl))

;;; Apply a Scheme->C rep-converter to the C expression EXP.
(define (C-ize info exp)
  (cond ((argrep:c-cvtr info)
	 => (lambda (s)
	      (if (string=? s "") exp
		  (string-append s "(" exp ")"))))
	(else exp)))

;;; Return a C statement rep-converting the C value VAL into the
;;; carrier CARRIER. Rep-conversion is determined by INFO.
(define (Scheme-ize->carrier info carrier val)
  (cond ((retrep:C-boxcvtr info)
	 => (lambda (f) (f carrier val)))
	(else (error "Rep is not carrier rep:" info))))

;;; Apply a C->Scheme rep-converter in the C stub to C expression EXP.
(define (Scheme-ize-exp converter exp)
  (if (string=? converter "") exp
      (string-append converter "(" exp ")")))

;;; If an arg needs post-C processing in the C stub, 
;;; then we need to assign the arg's C rep to a variable.
;;; Return #f or "    char *f3 = scm2c_string(arg[2]);"
(define (free-var-decl info fvar stub-arg)
  (and (argrep:post-C info)
       (format #f "~%    ~a = ~a;"
	       (format #f (argrep:c-decl info) fvar)
	       (C-ize info stub-arg))))


;;; Multiple return values happen across three boundaries: C routine -> C stub,
;;; C stub -> Scheme stub, and Scheme stub -> user. M.v. return happens
;;; across these boundaries sometimes for different reasons. If the
;;; C routine returns m.v., then everyone does. But even if the C routine
;;; returns just a single value, the C stub may rep-convert that multiple
;;; ways, and so need to pass multiple values back to the Scheme stub.

;;; Nomenclature: if someone is returning 4 return values, let's call
;;; the first value returned the *major return value*, and the other three
;;; values the *minor return values*.

;;; M.V. return linkages work like this:
;;; The C routine returns m.v.'s to the C stub by (1) returning the major value
;;; as the value of the C routine, and (2) assigning the minor return values
;;; to pointers passed to the C routine from the stub -- these pointer values
;;; are appended to the routine's parameter list after the actual arguments.
;;; That is, if the C routine needs to return an int, it will be passed
;;; an int*, which it assigns to return the int value.

;;; If the Scheme stub is expecting N multiple values, it passes in
;;; a Scheme vector of size N-1 to the C stub. The C stub stashes the
;;; minor return values into this vector; the major value is passed back
;;; as the C stub's actual return value. This vector is always the last
;;; value passed to the C stub from the Scheme stub, so we can get it
;;; in the C stub by accessing arg[0] or just *arg (remember, the args
;;; get their order reversed during the Scheme/C transition when they 
;;; are pushed on the Scheme48 stack, so the m.v. vector, being last, comes
;;; out first).
;;;
;;; If the major return value for the call requires a carrier structure,
;;; it is passed in the m.v. Scheme vector, in the first element of the
;;; vector. The carrier itself is returned as the C stub's major return value.

;;; MAKE-MV-ASSIGNS produces the C code that puts the C stub's minor
;;; return values into the vector. For each value and each rep for that value:
;;; - If the value is the major return value:
;;;   + if the value is immediate, it is rep-converted, and assigned to
;;;     the variable ret1.
;;;   + if the value is passed back in a carrier, the carrier is fetched
;;;     from the m.v. vector's elt 0, and the value is rep-converted into
;;;     this carrier. The carrier itself is assigned to ret1.
;;; - If the value is a minor return value:
;;;   + if the value is immediate, it is rep-converts, and assigned to
;;;     the appropriate slot in the m.v. vector.
;;;   + if the value is passed back in a carrier, the carrier is fetched
;;;     from the m.v. vector, and the value is rep-converted into the carrier.

;;; Ugh. Nested looping in Scheme is like nested looping in assembler.
(define (make-mv-assigns c-vars info-lists)
  (apply string-append
	 (let lp1 ((j 0) ; J is location in Scheme vec into which we store.
		   (c-vars c-vars)
		   (info-lists info-lists)
		   (assigns '()))
	   (if (pair? c-vars)
	       
	       (let ((v (car c-vars))
		     (info-list (car info-lists))
		     (c-vars (cdr c-vars))
		     (info-lists (cdr info-lists)))

		 ;; Loop over V's info elts in INFO-LIST
		 (let lp2 ((j j)
			   (info-list info-list)
			   (assigns assigns))
		   (if (pair? info-list)

		       ;; Do rep-conversion INFO.
		       (let ((info (car info-list))
			     (info-list (cdr info-list)))
			 (receive (c-stmt j)
			     (if (null? assigns)
				 (make-major-retval-stmt v info)
				 (make-minor-retval-stmt v info j))
			   (lp2 j info-list (cons c-stmt assigns))))

		       (lp1 j c-vars info-lists assigns))))

	       (reverse assigns)))))
;;; c-decl: 0,  immediate: 1, C-boxcvtr: 2,  make-carrier: 3,  s-cvtr: 4

;;; Major ret value rep conversion. If immediate, just rep-convert & assign
;;; to ret1. If carrier, store into an alien struct and assign that to ret1.
;;; C-VAR should always be "r1".
(define (make-major-retval-stmt c-var info)
  (cond ((retrep:immediate info) =>
	 (lambda (cvtr)
	   (values (format #f "~%    ret1 = ~a;" (Scheme-ize-exp cvtr c-var))
		   0)))
	(else
	 (values (format #f "~%    ret1 = S48_VECTOR_REF(mv_vec,0);~%    ~a"
			 (Scheme-ize->carrier info "ret1" c-var))
		 1))))

;;; Minor ret value rep-conversion.
;;; Convert and store into minor-value vector at entry j.
(define (make-minor-retval-stmt c-var info j)
  (values (cond ((retrep:immediate info) =>
                 (lambda (cvtr)
		   (format #f "~%    S48_VECTOR_SET(mv_vec,~d,~a);"
			   j (Scheme-ize-exp cvtr c-var))))
		(else
		 (let ((target (format #f "S48_VECTOR_REF(mv_vec,~d)" j)))
		   (format #f "~%    ~a"
			   (Scheme-ize->carrier info target c-var)))))
	  (+ j 1)))

(define (stmts strings) (apply string-append strings))

(define (make-post-C-var-list infos)
  (do ((j 1 (+ j 1))
       (infos infos (cdr infos))
       (ans '()
	    (cons (let ((i (car infos)))
		    (and (argrep:post-C i) (format #f "f~d" j)))
		  ans)))
      ((not (pair? infos)) (reverse ans))))


;;; Compute the args part of function prototype.
(define (proto-args arg-decls)
  (if (null? arg-decls) "void" ; echh
      (elts->comma-string arg-decls)))


(define (define-foreign->C-stub form)
  (destructure (( (#f scheme-name (c-name . params) . return-reps) form ))
    (let* ((c-name (stringify c-name))
	   (reps (map car params))
	   (no-declare? (and (pair? return-reps)
			     (eq? 'no-declare (car return-reps))))
	   (return-reps (if no-declare? (cdr return-reps)
			    return-reps))

	   (params-info (map (lambda (rep)
			       (parameter-rep->info rep (lambda (x) x)))
			     reps))
	   ;; A list of lists, due to MULTI-REP.
	   (ret-infos1 (parse-return-reps return-reps
					  (lambda (x) x)))
	   (ignore? (string? ret-infos1))
	   (gc-protected-vars (if ignore?  ; maybe extended by mv_vec
				  '() 
				  '("ret1")))

	   (ret-infos2 (if (not ignore?)	; Flatten them out.
			   (apply append ret-infos1))) 
	   (ret-infos3 (if (not ignore?)	; A canonical representative
			   (map car ret-infos1)))    ; per item.
	   
	   (primary-retval-info (if (not ignore?) (car ret-infos3)))
	   (primary-retval-decl-template
	    (if ignore?
		ret-infos1
		(retrep:c-decl primary-retval-info)))
	   ;; The type of the value returned by the C routine,
	   ;; stored into the C stub's r1 variable.
	   (primary-retvar-decl (if ignore? ""
				    (format #f "~%    ~a;"
					    (format #f primary-retval-decl-template
						    "r1"))))
	   (mv-return? (and (not ignore?)
			    (or (pair? (cdr ret-infos2))
				;; Is major ret val non-immediate
				(not (retrep:immediate
				      (caar ret-infos1))))))
	   (gc-protected-vars (if mv-return? 
				  (cons "mv_vec" gc-protected-vars)
				  gc-protected-vars))
	   (gc_declare (if (> (length gc-protected-vars) 0) 
			   (format #f "~%    S48_DECLARE_GC_PROTECT(~d);" 
			       (length gc-protected-vars))
			   ""))
	   (gc_protect (if (> (length gc-protected-vars) 0)
			   (format #f "~%    S48_GC_PROTECT_~d(~a);"
			       (length gc-protected-vars)
			       (if (null? (cdr gc-protected-vars))
				   (car gc-protected-vars)
				   (string-append (car gc-protected-vars)
						  ","
						  (cadr gc-protected-vars))))
			   ""))
	   (nargs (length reps))
	   (stub-nargs (if mv-return? (+ nargs 1) nargs))
	   (other-retvals (if ignore? '() (cdr ret-infos3)))
	   (ret-vars (map (make-gensym "r" 1) other-retvals))
	   (ret-var-decls (stmts (map info->var-decl 
				      other-retvals ret-vars)))
	   
	   ; Frank: begin
	   (gensym (let ((gs (make-gensym "g" 0)))
		     (lambda () (gs #f))))
	   ; the c-stubs formal parameters are named "g1" ... "gn"
	   (stub-args (map (lambda (p) (gensym)) params))
	   ; Frank: end
	   (post-C-vars (make-post-C-var-list params-info))
	   (pc-var-decls (stmts (map (lambda (i v)
				       (if v (info->var-decl i v) ""))
				     params-info
				     post-C-vars)))
	   
	   (c-proto (proto-args (append (map info->type params-info)
					(map (lambda (i)
					       (info->type i "*"))
					     other-retvals))))
	   
	   (c-fun-decl (format #f primary-retval-decl-template
			       (string-append c-name "(" c-proto ")")))
	   (c-fun-decl (format #f "extern ~a;" c-fun-decl))
	   (c-fun-decl (if no-declare? "" c-fun-decl))
	   
	   (pc-var-assigns (stmts (map (lambda (i fv sv)
					 (if fv
					     (format #f "~%    ~a = ~a;"
						     fv (C-ize i sv))
					     ""))
				       params-info
				       post-C-vars
				       stub-args)))
	   
	   (c-args (elts->comma-string (append (map (lambda (i fv sv)
						      (or fv (C-ize i sv)))
						    params-info
						    post-C-vars
						    stub-args)
					       (map (lambda (rv)
						      (string-append "&" rv))
						    ret-vars))))
	   (c-call (string-append c-name "(" c-args ")"))
	   
	   ;; Do the post-C-call processing in the C stub.
	   (post-C-val-processing
	    (stmts (map (lambda (v i)
			  (if v
			      (format #f "~%    %a(~a);"
				      (argrep:post-C i) v)
			      ""))
			post-C-vars reps)))


	   (mv-assigns (if ignore? ""
			   (make-mv-assigns (cons "r1" ret-vars)
					    ret-infos1)))
	   (gc_unprotect (if (> (length gc-protected-vars) 0)
			     "\n    S48_GC_UNPROTECT();"
			     ""))
	   (return-stmt (format #f "~%    return ~a;"
				(if ignore? "S48_FALSE" "ret1")))

	   ;; Do the call, release the free-vars, do the mv-return
	   ;; assignments, then return.
	   (epilog (if ignore?
		       (string-append  c-call ";" post-C-val-processing return-stmt)
		       (string-append  gc_protect "\n    r1 = " c-call ";"
				      post-C-val-processing
				      mv-assigns gc_unprotect return-stmt))))
;     (breakpoint)
      (format #f cfun-boilerplate
	      c-name
	      ; Frank: begin
	      ; multiple values will be returned in the c-stubs last formal parameter:
	      ; the vector mv_vec
	      (let ((args (if mv-return? (append stub-args '("mv_vec")) stub-args)))
		(proto-args (map (lambda (var) (string-append "s48_value " var)) args)))
	      ; Frank: end
	      c-fun-decl
	      (if ignore? "" ret1-decl)
	      gc_declare
	      primary-retvar-decl ret-var-decls pc-var-decls
	      pc-var-assigns
	      epilog))))

; Frank: begin
(define cfun-boilerplate
  "s48_value df_~a(~a)
{
    ~a~a~a~a~a~a

    ~a
    ~a
}

")
; Frank: end

(define ret1-decl
  "
    s48_value ret1 = S48_FALSE;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cfile-header-boilerplate
  "/* This is an Scheme48/C interface file, 
** automatically generated by a hacked version of cig 3.0.
step 4
*/

#include <stdio.h>
#include <stdlib.h> /* For malloc. */
#include \"libcig.h\"

")

; Frank: begin

(define s48-init-boilerplate
  "void s48_init_~a(void)
{~a
}
")

(define (define-foreign-process-form form oport)
  ; c-names will be the list of c-names of all define-foreign-forms
  (define c-names '())
  (define init-name #f)
  (define (define-foreign-process-form2 form)
    (if (pair? form)
	(case (car form)
	  
	  ((begin)
	   (if (list? (cdr form))
	       (for-each (lambda (f) (define-foreign-process-form2 f))
			 (cdr form))))
	  
	  ((define-structure define-structures)
	   (if (and (pair? (cdr form))
		    (list? (cddr form)))
	       (let ((clauses (cddr form)))
		 (for-each (lambda (clause)
			     (if (and (pair? clause)
				      (eq? 'begin (car clause)))
				 (define-foreign-process-form2 clause)))
			   clauses))))
	  
	  ((define-foreign)
	   (let ((c-name (string-append "df_" (stringify (caaddr form)))))
	     (begin
	       (set! c-names (cons c-name c-names))
	       (display (define-foreign->C-stub form) oport))))

	  ((foreign-init-name)
	   (let ((name (cdr form)))
	     (set! init-name (car name))))

	  ((define-stubless-foreign)
	   (let ((c-name (cadddr form)))
	     (set! c-names (cons c-name c-names))))
	    
	  ((foreign-source)
	   (let ((forms (cdr form)))
	     (if (pair? forms)
		 (begin (display (car forms) oport)
			(map (lambda (x)
			       (newline oport)
			       (display x oport))
			     (cdr forms)))))))))
  (define-foreign-process-form2 form)
  (values (reverse c-names) init-name))

; Frank: end
(define (display-register c-names init-name oport)
  (if (not init-name)
      (error "no foreign-init-name statement found")
      (let ((register-txt
	     (apply
	      string-append
	      (map (lambda (c-name)
		     (format #f "~%    S48_EXPORT_FUNCTION(~a);" c-name))
		   c-names))))
	(format oport s48-init-boilerplate init-name register-txt))))

(define (process-define-foreign-stream iport oport)
  (display cfile-header-boilerplate oport)
  (let lp ((c-names '()) (init-name #f))
    (let ((form (read iport)))
      (if (eof-object? form)
	  (display-register c-names init-name oport)
	  (receive (new-c-names maybe-init-name)
		   (define-foreign-process-form form oport)
		   (let ((init-name (if maybe-init-name 
					(if init-name
					    (error "multiple foreign-init-name definitions")
					    maybe-init-name)
					init-name)))
		     (lp (append c-names new-c-names) init-name)))))))

; Frank: begin
; (process-define-foreign-file fname) scans file fname.scm and produces a c-stub for every
; scanned define-foreign form and places it in file fname.c.
(define (process-define-foreign-file fname init-name)
  (call-with-input-file (string-append fname ".scm")
    (lambda (iport)
      (call-with-output-file (string-append fname ".c")
	(lambda (oport)
	  (begin
	    (display cfile-header-boilerplate oport)
	    (let lp ((c-names '()))
	      (let ((form (read iport)))
		(if (eof-object? form)
		    (let ((register-txt
			   (apply
			    string-append
			    (map (lambda (c-name)
				   (format #f "~%    S48_EXPORT_FUNCTION(~a);" c-name))
				 c-names))))
		      (format oport s48-init-boilerplate init-name register-txt))
		    (lp (append c-names (define-foreign-process-form form oport))))))))))))
; Frank: end


(define (cig-standalone-toplevel f-and-init-name) ; ignore your args no
  (process-define-foreign-stream (current-input-port)
				 (current-output-port))
  0)

;;; This section defines the Scheme-side macro processor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (define-syntax define-foreign define-foreign-expander)

(define (define-foreign-expander form rename compare)
  (destructure (( (#f scheme-name (c-name . params) . return-reps) form ))
    (let* ((c-name (string-append "df_" (stringify c-name)))

	   (reps (map car params))
	   (params-info (map (lambda (rep) (parameter-rep->info rep rename))
			     reps))

	   (return-reps (if (and (pair? return-reps)
				 (eq? 'no-declare (car return-reps)))
			    (cdr return-reps)
			    return-reps))
	   (ret-infos1 (parse-return-reps return-reps rename))
	   (ignore? (string? ret-infos1))

	   (ret-infos2 (if (not ignore?)
			   (apply append ret-infos1)))
	   (major-rep (and (not ignore?) (car ret-infos2)))

	   ;; Does the Scheme stub return m.v.'s to the user?
	   (scheme-mv-return? (and (not ignore?)
				   (pair? (cdr ret-infos2))))

	   (carrier-vec? (or scheme-mv-return?
			     (and major-rep
				  (not (retrep:immediate major-rep)))))
	     
	   (carrier-veclen (if carrier-vec?
			       (- (length ret-infos2)
				  (if (retrep:immediate major-rep) 1 0))))

	   (%define (rename 'define))
	   (%let (rename 'let))
	   (%lambda (rename 'lambda))
	   ; JMG: begin replaced external-lambda by import-lambda-definition
	   (%import-lambda-definition (rename 'import-lambda-definition))
	   (%lookup-imported-binding (rename 'lookup-imported-binding))
	   (%call-imported-binding (rename 'call-imported-binding))
	   (%let* (rename 'let*))
	   ; JMG: end
	   (gensym (let ((gs (make-gensym "g" -1)))
		     (lambda () (string->symbol (gs #f)))))

	   (args (map (lambda (p)
			(let ((tail (cdr p)))
			  (if (pair? tail) (car tail)
			      (gensym))))
		      params))

	   (%string?   (rename 'string?))
	   (%char?     (rename 'char?))
	   (%integer?  (rename 'integer?))
	   (%vector?   (rename 'vector?))
	   (%pair?   (rename 'pair?))
	   (%check-arg (rename 'check-arg))

	   (rep-checker (lambda (i arg)
			  (cond ((argrep:scheme-pred i) =>
				 (lambda (pred) `(,%check-arg ,pred ,arg
							      ,scheme-name)))
				(else arg))))

	   (c-args (map rep-checker params-info args))
	   (%f (rename 'f)))

      (if (not carrier-vec?)
	  (let* ((xcall `(,%f ,@c-args))
		 (xcall (cond ((and (not ignore?)
				    (retrep:s-cvtr (car ret-infos2)))
			       => (lambda (proc) `(,proc ,xcall))) ; not hygenic
			      (else xcall))))

	    ; Frank: begin
	    ; get-external and external-call replaced: now external-lambda
	    ;JMG now import-lambda-definition
	    `(,%import-lambda-definition ,scheme-name ,args ,c-name))
	   (let ((retarg1 (rename 'r1))
		(retarg2 (rename 'r2))
		(%make-vector (rename 'make-vector)))
	    `(,%define ,scheme-name
	       (,%let ((,%f (,%lookup-imported-binding ,c-name)))
	         (,%lambda ,args
		   (,%let ((,retarg2 (,%make-vector ,carrier-veclen)))
		     ,@(install-carriers retarg2 ret-infos2
					 (rename 'vector-set!))
		     (,%let ((,retarg1 (,%call-imported-binding ,%f ,@c-args ,retarg2)))
		       (values ,@(make-values-args retarg1 retarg2
						   ret-infos2
						   rename))))))))))))
;;	  `(,%define ,scheme-name
;;	     (,%let ((,%f (,%external-lambda ,args ,c-name)))
;;	       (,%lambda ,args ,xcall))))

;	  (let ((retarg1 (rename 'r1))
;		(retarg2 (rename 'r2))
;		(%make-vector (rename 'make-vector)))
;	    `(,%define 
;	      ,scheme-name 
;	      (,%let* ((temp (,%lookup-imported-binding ,c-name))
;		       (,%f (,%lambda ,args (,%call-imported-binding temp ,args))))
;		      (,%lambda ,args
;				(,%let ((,retarg2 (,%make-vector ,carrier-veclen)))
;				       ,@(install-carriers retarg2 ret-infos2
;							   (rename 'vector-set!))
;				       (,%let ((,retarg1 (,%f ,@c-args ,retarg2)))
;					      (values ,@(make-values-args retarg1 retarg2
;									  ret-infos2
;									  rename))))))))))))
; Frank: end
(define (install-carriers carrier-vec ret-infos2 %vector-set!)
  ;; Skip the major ret value if it doesn't require a carrier.
  (let* ((major-rep (and (pair? ret-infos2) (car ret-infos2)))
 	 (infos (if (and major-rep (retrep:immediate major-rep))
 		    (cdr ret-infos2)
 		    ret-infos2)))
    
    (let lp ((ans '()) (infos infos) (i 0))
      (if (null? infos) ans
 	  (let ((info (car infos))
 		(infos (cdr infos)))
 	    (if (retrep:immediate info)
 		(lp ans infos (+ i 1))
 		(lp (cons `(,%vector-set! ,carrier-vec ,i
 					  (,(retrep:make-carrier info)))
 			  ans)
 		    infos
 		    (+ i 1))))))))

(define (c-arg i retarg1 retarg2 %vector-ref)
  (if (zero? i)
      retarg1
      `(,%vector-ref ,retarg2 ,(- i 1))))

(define (make-values-args arg1 carrier-vec infos rename)
  (let ((%vector-ref (rename 'vector-ref))
	(do-arg (lambda (arg info)
		  (cond ((retrep:s-cvtr info) =>
			 (lambda (cvtr) `(,cvtr ,arg)))
			(else arg)))))
    (if (null? infos) '()
	(let lp ((ans (list (do-arg arg1 (car infos))))
		 (i (if (retrep:immediate (car infos)) 0 1))
		 (infos (cdr infos)))
	  (if (pair? infos)
	      (let* ((info (car infos))
		     (arg `(,%vector-ref ,carrier-vec ,i)))
		(lp (cons (do-arg arg info) ans)
		    (+ i 1)
		    (cdr infos)))
	      (reverse ans))))))

)) ; egakcap



(define-structure define-foreign-syntax 
  (export (define-foreign :syntax)
	  (foreign-source :syntax)
	  (foreign-init-name :syntax)
	  (define-stubless-foreign :syntax))
  (open scheme external-calls  structure-refs cig-aux)
  (access signals) ; for ERROR
  (for-syntax (open scheme define-foreign-syntax-support))
  (begin
    (define error (structure-ref signals error))

    (define-syntax define-foreign define-foreign-expander)

    ;; Ignore FOREIGN-SOURCE forms.
    (define-syntax foreign-source
      (syntax-rules ()
	((foreign-source stuff ...) #f)))

    (define-syntax foreign-init-name
      (syntax-rules ()
	((foreign-init-name name) (lambda () name))))

    (define-syntax define-stubless-foreign
      (syntax-rules ()
         ((define-stubless-foreign bla ...)
	  (import-lambda-definition bla ...))))

    (define (check-arg pred obj proc)
      (if (not (pred obj))
	  (error "check-arg" pred obj proc)
	  obj))
)) ; egakcap


;;; Todo: "info" terminology is gone. Clean up.











































