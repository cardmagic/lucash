; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; This is file arch.scm.

;;;; Architecture description

(define architecture-version "Vanilla 20")

; Things that the VM and the runtime system both need to know.

(define bits-used-per-byte 8)

(define byte-limit (expt 2 bits-used-per-byte))

; Bytecodes: for compiler and interpreter

; Instruction specification is 
; (op . args)
; OP may be a name or a list of names
; ARGS are
;  nargs     - a byte
;  byte      - a byte
;  junk	     - a byte that is ignored (e.g. when a peephole optimization merges
;              two instructions into a single, shorter one)
;  two-bytes - two bytes
;  index       - a two byte index into the current template
;  small-index - a one byte index into the current template
;  offset    - two bytes giving an offset into the current instruction stream
;  stob      - a byte specifying a type for a stored object
;  0 1 2 ... - the number of non-instruction-stream arguments (some
;              instructions take a variable number of arguments; the first
;              number is the argument count implemented by the VM)
;  +         - any number of additional arguments are allowed

(define-syntax define-instruction-set
  (lambda (form rename compare)
    (let ((data (do ((data (reverse (cdr form)) (cdr data))
		     (new '() (let ((next (car data)))
				(if (pair? (car next))
				    (append (map (lambda (op)
						   (cons op (cdr next)))
						 (car next))
					    new)
				    (cons next new)))))
		    ((null? data) new))))
      `(begin (define-enumeration op
		,(map car data))
	      (define opcode-arg-specs
		'#(,@(map cdr data)))))))

; Instructions marked *EXP* are experimental and are not normally used by
; byte-code compiler.

(define-instruction-set
  (protocol       protocol)      ; first opcode in a procedure, never actually
				 ; executed

  (make-env       two-bytes)     ; cons an environment

  (literal        index)         ; value to *val*, two-byte index
  (small-literal  small-index)   ; value to *val*, one-byte index
  (local          byte byte)     ; back and over
  ((local0 local1 local2)
   byte)		         ; back encoded into op-code for efficiency
  (big-local      two-bytes two-bytes)   ; same, but counts are two bytes
  (set-local!     two-bytes two-bytes 1) ; back over value, counts are two bytes
  (global         index)         ; value to *val*
  (set-global!    index 1)
  (closure        index byte)    ; byte = 0 -> use environment in *env*
                                 ; byte = 1 -> use environment in *val*
  (make-flat-env  env-data)      ; make new environment from env-data
  (push 1)		         ; push *val* onto stack
  ((local0-push push-local0)	 ; common combination
		  byte junk 1)
  (pop)			         ; pop top of stack into *val*
  (stack-ref      byte)	         ; index'th element of stack into *val*
  (stack-set!     byte 1)        ; *val* to index'th element of stack

  (make-cont      offset byte)      ; save state in *cont*
  (make-big-cont  offset two-bytes) ; save state in *cont*, two-byte size
  (current-cont)	         ; copy *cont* to *val*, use WITH-CONTINUATION
			         ; to use copied continuation
  (get-cont-from-heap)	         ; copy next continuation from heap (this
			         ; op-code is used internally by the VM)

  ;; different ways to call procedures
  (call     nargs     1 +)       ; last argument is the procedure to call
  (big-call two-bytes 1 +)       ; ditto, nargs count is two bytes
  (move-args-and-call nargs 1 +) ; same, move args to just above *cont* first
  				 ; (*EXP*, and no two-byte version)
  (apply    two-bytes 2 +)       ; last argument is procedure to call, second to
				 ; last is a list of additional arguments, next
                                 ; two bytes are the number of stack arguments
  (closed-apply 2 +)		 ; arguments are as for Scheme's APPLY, with
				 ; the number of non-list arguments pushed on
  				 ; the top of the stack
  (with-continuation 2)		 ; first arg is cont, second is procedure
  (call-with-values +)		 ; values are on stack, consumer is in the
				 ; continuation pointed to by *cont*

  ;; Three different ways to return from calls and one way to ignore any
  ;; returned values
  (return 1)			 ; return to continuation in *cont*
  (values two-bytes +)		 ; values are on stack, count is next two bytes
  (closed-values +)		 ; values are on stack, count is pushed on stack
  (ignore-values +)		 ; ignore (and dispose of) returned values

  ;; Different ways to jump
  (goto-template        index)	 ; jump to another template (*EXP*)
				 ; does not poll for interrupts
  (call-template  index nargs)	 ; call a template instead of a procedure
				 ; nargs is needed for interrupt handling
  				 ; Current VM only handles the zero-arg case.
  (jump-if-false  offset 1)	 ; boolean in *val*
  (jump           offset)
  (computed-goto  byte offset 1) ; jump using delta specified by *val*
				 ; defaults to instruction after deltas (*EXP*)

  ;; For the closed-compiled definitions of n-ary arithmetic functions.
  ;; The opcode sequences used are:
  ;;       binary-reduce1 binary-op binary-reduce2 return
  ;; and
  ;;       binary-reduce1 binary-op binary-comparison-reduce2 return
  ((binary-reduce1 binary-reduce2 binary-comparison-reduce2))

  ;; Scalar primitives
  (eq? 2)

  ((number? integer? rational? real? complex? exact?) 1)
  ((exact->inexact inexact->exact) 1)

  ((+ *) 2 0 1 +)
  ((- /) 2 1)
  ((= < > <= >=) 2 +)
  ((quotient remainder) 2)
  ((floor numerator denominator
     real-part imag-part
     exp log sin cos tan asin acos sqrt
     angle magnitude)
   1)
  (atan 2)
  ((make-polar make-rectangular) 2)
  (bitwise-not 1)
  ((bitwise-and bitwise-ior bitwise-xor) 2)
  (arithmetic-shift 2)
  (char? 1)
  ((char=? char<?) 2)
  ((char->ascii ascii->char) 1)
  (eof-object? 1)

  ;; Data manipulation
  (stored-object-has-type? stob 1)
  (stored-object-length stob 1)

  (make-stored-object byte stob)
  (closed-make-stored-object stob) ; size pushed on stack
  (stored-object-ref  stob byte 1) ; byte is the offset
  (stored-object-set! stob byte 2)

  (make-vector-object stob 2)	      ; size + init
  (stored-object-indexed-ref  stob 2) ; vector + offset
  (stored-object-indexed-set! stob 3) ; vector + offset + value

  (make-byte-vector 2)
  (byte-vector-length 1)
  (byte-vector-ref 2)
  (byte-vector-set! 3)

  (make-string 2)
  (string-length 1)
  (string-ref 2)
  (string-set! 3)

  (intern 1)
                     
  (location-defined? 1)
  (set-location-defined?! 2)
  ((immutable? make-immutable!) 1)

  ;; channels (unbuffered, non-blocking I/O)
  (open-channel 2)
  (close-channel 1)
  (channel-maybe-read 5)
  (channel-maybe-write 4)
  (add-pending-channel 2)
  (channel-ready? 1)
  (channel-abort 1)             ; stop channel operation
  (open-channels-list)		; return a list of the open channels
  
  ;; Misc
  ((unassigned unspecific))
  (trap 1)			; raise exception
  (false)			; return #f (for bootstrapping)
  (eof-object)                  ; hard to get otherwise
  (write-image 3)
  (collect)
  (string-hash 1)		; used by the static linker for the initial table
  (add-finalizer! 2)
  (memory-status 2)
  (find-all 1)	                ; makes a vector of all objects of a given type
  (find-all-records 1)          ; makes a vector of all records of a given type
  (current-thread)
  (set-current-thread! 1)
  (session-data)                ; session specific data
  (set-session-data! 1)
  (set-exception-handlers! 1)
  (return-from-exception 1)
  (set-interrupt-handlers! 1)
  (set-enabled-interrupts! 1)
  (return-from-interrupt)
  (schedule-interrupt 1)
  (wait 2)                      ; do nothing until something happens
  (call-external-value 1 +)
  (lookup-shared-binding 2)
  (define-shared-binding 3)
  (undefine-shared-binding 2)
  (time 2)
  (vm-extension 2)		; access to extensions of the virtual machine
  (return-from-callback 2)	; return from an callback

  ;; Unnecessary primitives
  (string=? 2)
  (reverse-list->string 2)
  (assq 2)
  (checked-record-ref 3)
  (checked-record-set! 4)
  (copy-bytes! 5)

  ;; ports (buffered I/O) - these are all unnecessary
  ;; byte = 0 -> port is supplied
  ;;      = 1 -> get port from dynamic environment
  ((read-char peek-char) byte 1 0)
  (write-char byte 2 1)

  ;; For writing informative messages when debugging
  (message 1)
  )

(define-enumeration interrupt
  (alarm           ; order matters - higher priority first
   keyboard
   post-gc         ; handler is passed a list of finalizers
   i/o-read-completion  ; handler is passed channel and status
   i/o-write-completion  ; handler is passed channel and status
   os-signal       
   ))

; Possible problems

(define-enumeration exception
  (unassigned-local
   undefined-global
   unbound-global
   bad-procedure
   wrong-number-of-arguments
   wrong-type-argument
   arithmetic-overflow
   index-out-of-range
   heap-overflow
   out-of-memory
   cannot-open-channel
   channel-os-index-already-in-use
   closed-channel
   pending-channel-i/o
   buffer-full/empty
   unimplemented-instruction
   trap
   proceeding-after-exception
   bad-option
   unbound-external-name
   too-many-arguments-to-external-procedure
   too-many-arguments-in-callback
   callback-return-uncovered
   extension-exception
   extension-return-error
   os-error
   unresumable-records-in-image
   gc-protection-mismatch
   ))

; Used by (READ-CHAR) and (WRITE-CHAR) to get the appropriate ports from
; the fluid environment.

(define-enumeration current-port-marker
  (current-input-port
   current-output-port))

;----------------
; Encoding for template protocols:
; 0 ... MAX-STACK-ARGS = that number of arguments, no rest list
; TWO-BYTE-NARGS = (2*MAX-STACK-ARGS)+1 = next two bytes are the fixed argument
;  count
; TWO-BYTE-NARGS+LIST = TWO-BYTE-NARGS + 1 = next two bytes are the fixed
;  argument count, plus a rest list

(define maximum-stack-args 63)

(define *last-protocol* maximum-stack-args)

(define (next-protocol)
  (set! *last-protocol* (+ *last-protocol* 1))
  *last-protocol*)

(define two-byte-nargs-protocol      (next-protocol))

; Used for all n-ary procedures.

(define two-byte-nargs+list-protocol (next-protocol))

; Real protocol is at the end of the code vector, along with the required
; stack size:
;   ... real-protocol stack-size0 stack-size1
; This stuff has to be at the end of the code vector because the necessary stack
; size is not determined until after the code vector has been assembled.

(define big-stack-protocol (next-protocol))

; The rest are used only for the definitions of various Scheme primitives.

; For VECTOR, RECORD, VALUES, EXTERNAL-CALL, APPLY
; Next byte is the minimum number of arguments (1 for EXT-CALL, 2 for APPLY,
; 0 for the rest).
; Stack = arg0 arg1 ... argN rest-list N+1 total-arg-count
; The first two arguments are always on the stack.

(define args+nargs-protocol (next-protocol))

; Followed by four bytes: the offsets of code for the 0, 1, 2, and 3+ arg cases.
; A zero indicatest that the primitive doesn't accept that many arguments.
; If there are fewer than three arguments they are all on the stack.  In the
; 3+ case this is the same as args+nargs above.

(define nary-dispatch-protocol (next-protocol))

; The maximum number of arguments that can be passed to EXTERNAL-CALL.
; This is determined by the C procedure `external_call()'.

(define maximum-external-call-args 12)

;----------------
; The number of stack slots available to each procedure by default.
; Procedures that need more than this must use one of the two-byte-nargs
; protocols.  All of these are given in terms of descriptors.

(define default-stack-space 64)

(define environment-stack-size 2)   ; header + superior environment
(define continuation-stack-size 5)  ; header + continuation + template +
                                    ;  pc + env

(define available-stack-space 8000) ; how much stack space is available for
                                    ; any one procedure

;----------------

; Options for op/time

(define-enumeration time-option
  (run-time
   real-time
   cheap-time     ; cheap (no system call) access to the polling clock
   ;current-time
   ))

; Options for op/memory-status

(define-enumeration memory-status-option
  (available
   heap-size
   stack-size
   gc-count
   expand-heap!
   pointer-hash
   ))

; The two types of special channels cannot be used for normal I/O.

(define-enumeration channel-status-option
  (closed
   input
   output
   special-input   ; socket accept, ???
   special-output  ; ???
   ))

; Indicies into a port's status word

(define-enumeration port-status-options
  (input
   output
   open-for-input
   open-for-output
   ))

(define-enumeration stob
  (;; D-vector types (traced by GC)
   pair
   symbol
   vector
   closure
   location
   cell
   channel
   port
   ratnum
   record
   continuation
   extended-number
   template
   weak-pointer
   shared-binding
   unused-d-header1
   unused-d-header2

   ;; B-vector types (not traced by GC)
   string        ; = least b-vector type
   byte-vector
   double        ; double precision floating point
   bignum
   ))

; This is here to try to ensure that it is changed when STOB changes.
(define least-b-vector-type (enum stob string))

; (stob predicate constructor . (accessor modifier)*)
; If nothing else, the run-time system and the VM need to agree on
; which slot of a pair is the car and which is the cdr.

(define stob-data
  '((pair pair? cons
      (car set-car!) (cdr set-cdr!))
    (symbol symbol? #f       ; RTS calls op/string->symbol
      (symbol->string))
    (location location? make-location
      (location-id set-location-id!)
      (contents set-contents!))
    (cell cell? make-cell
      (cell-ref cell-set!))
    (closure closure? make-closure
      (closure-template) (closure-env))
    (weak-pointer weak-pointer? make-weak-pointer
      (weak-pointer-ref))
    (shared-binding shared-binding? make-shared-binding
      (shared-binding-name)
      (shared-binding-is-import?)
      (shared-binding-ref shared-binding-set!))
    (port port? make-port
      (port-handler set-port-handler!)
      (port-status  set-port-status!)
      (port-lock    set-port-lock!)
      (port-locked? set-port-locked?!)
      (port-data    set-port-data!)
      (port-buffer  set-port-buffer!)
      (port-index   set-port-index!)
      (port-limit   set-port-limit!)
      (port-pending-eof? set-port-pending-eof?!))
    (channel channel? #f
      (channel-status)
      (channel-id)
      (channel-os-index))
    ))

