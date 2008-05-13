; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Miscellaneous things.

(define-structure reset/shift (export (reset :syntax)
				      (shift :syntax))
  (open scheme escapes signals)
  (files shift-reset))

(define-structure unix-system-calls
    (export pipe waitpid fork dup close execv exit)
  (open scheme externals signals)
  (files syscall))

(define-structure call-with-mumble-pipes
    (export call-with-input-pipe call-with-output-pipe)
  (open scheme
	unix-system-calls
	architecture channels
	i/o-internal
	signals)
  (files pipe))

(define-structure unix-getenv (export getenv)
  (open scheme primitives)
  (files getenv))

(define-structure nondeterminism (export (either :syntax)
					 (one-value :syntax)
					 (all-values :syntax)
					 fail
					 init)
  (open scheme)
  (files either))

(define-structure engines (export (engine :syntax))
  (open scheme primitives interrupts)
  (files engine))

(define-structure remote (export remote-repl
				 serve
				 make-socket)
  (open scheme
	;; For server
	closures
	handle conditions
	vm-exposure
	;; For client
	compiler
	command-processor
	interfaces
 	environments
	;; For both?
	packages
	package-commands-internal
	;; packages-internal ?
	syntactic
	built-in-structures
	dump/restore
	sockets
	signals features)
  (files remote))

(define-structure requirements (export (require :syntax))
  (open scheme-level-2
	packages
	environments
	ensures-loaded
	package-commands-internal
	signals)
  (files require))


; Procedure annotations

(define-structure annotations
    (export annotate-procedure procedure-annotation)
  (open scheme-level-1 assembler)
  (files annotate))


; DOODL

(define-structure doodl doodl-interface
  (open scheme
	methods
	meta-methods
	annotations
	define-record-types
	records
	;; handle	; ignore-errors
	;; conditions	; error?
	util 
	signals)
  (files doodl))


(define-interface doodl-interface
  (export ((method
	    define-class
	    define-method
	    define-generic-function
	    define-class
	    set)
	   :syntax)
	  <function>
	  <generic-function>
	  <method>
	  <class>

	  <object>
	  <number>
	  <complex>
	  <real>
	  <rational>
	  <integer>
	  <pair>
	  <symbol>
	  <char>
	  <null>
	  <vector>
	  <string>
	  <eof-object>
	  <input-port>
	  <output-port>

	  <list>    ;foo
	  make
	  initialize
	  car-setter cdr-setter vector-ref-setter
	  id?
	  (bind :syntax)
	  ;; etc. etc.
	  ))
