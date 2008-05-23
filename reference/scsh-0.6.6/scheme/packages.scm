; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; Meta-modules: the big picture.

; Various implementations of Primitive Scheme.

(define-structure low-structures low-structures-interface
  (open meta-module-system the-interfaces)
  (files low-packages))

(define-structure debug-low-structures low-structures-interface
  (open meta-module-system the-interfaces
	;; built-in-structures
	)
  (files (alt low-packages)))


; Usual Scheme 48 run-time system.

(define (make-run-time low-structures)
  (structures (run-time-structures-interface
	       run-time-internals-structures-interface
	       features-structures-interface)
    (open meta-module-system the-interfaces
	  low-structures)
    (files rts-packages)))


; Alternatives to the usual Scheme 48 run-time system.

(define-structure alt-features-structures features-structures-interface
  (open meta-module-system the-interfaces)
  (files (alt features-packages)))

(define-structure cheat-features-structures features-structures-interface
  (open meta-module-system the-interfaces)
  (begin (define-structures ((ascii ascii-interface)
			     (bitwise bitwise-interface)
			     (code-vectors code-vectors-interface)
			     (features features-interface)
			     (records records-interface)
			     (signals signals-interface))
	   ;; Assumes use of FLATLOAD.  The implementations of these
	   ;; structures will become available via some miracle, e.g.
	   ;; a command ",open signals ... code-vectors" or an
	   ;; explicit LOAD of something.  Cf. the rule for
	   ;; link/linker.image in the Makefile.
	   )))

(define-module (make-alt-run-time features-structures)
  (structures (low-structures-interface
	       run-time-structures-interface)
    (open meta-module-system the-interfaces
	  features-structures)
    (files alt-packages
	   (alt low-packages))))


; Byte-code compiler and related things.

(define-module (make-compiler-structures run-time-structures
					 features-structures)

  (define-structure compiler-structures compiler-structures-interface
    (open meta-module-system the-interfaces
	  run-time-structures
	  features-structures)
    (files comp-packages))

  compiler-structures)


; The initial system (initial.image).  Cf. the rule for initial.image
; in the Makefile.

(define (make-initial-structures low-structures
				 run-time-structures
				 run-time-internals-structures
				 features-structures
				 compiler-structures)
  (structure initial-structures-interface
    (open meta-module-system the-interfaces
	  low-structures		;Cf. desirable-structures
	  run-time-structures
	  features-structures
	  run-time-internals-structures
	  compiler-structures)
    (files initial-packages)))


; Small systems.

(define-structure (make-debug-structures low-structures
					 run-time-structures
					 run-time-internals-structures
					 features-structures
					 initial-structures)
  (structure debug-structures-interface
    (open meta-module-system the-interfaces
	  low-structures
	  run-time-structures
	  run-time-internals-structures
	  features-structures
	  initial-structures)
    (files debug-packages)))


; Static linker.

(define-module (make-linker-structures features-structures
				       run-time-structures
				       compiler-structures)

  (define-structure linker-structures linker-structures-interface
    (open meta-module-system the-interfaces
	  features-structures
	  run-time-structures
	  compiler-structures)
    (files link-packages))

  linker-structures)



; The following definition of THE-INTERFACES assumes that we're
; "flatloading."  If we were really using the module system, then its
; interface would have to include all of the interfaces defined in
; interfaces.scm, and it would need a (files interfaces) clause.

(define-structure the-interfaces the-interfaces-interface
  (open )
  ;; (files interfaces)
  )
(define-interface the-interfaces-interface
  (export scheme-level-0-interface
	  primitives-interface
	  ;; ... etc. etc. ad nauseum
	  for-reification-interface))

; This definition of META-MODULE-SYSTEM assumes that we're flatloading.
; If we weren't, it would have to be
;   (def meta-module-system module-system)
; instead.

(define-structure meta-module-system (export ) (open ))  ;Kludge



; --------------------
; Particular assemblies:

; The usual run-time system (for initial.image, etc.).

(def run-time-structures run-time-internals-structures features-structures
  (make-run-time low-structures))


; The byte-code compiler as constituted for initial.image and friends.

(def compiler-structures
  (make-compiler-structures run-time-structures
			    features-structures))


; The initial system made in the usual way.

(def initial-structures
  (make-initial-structures low-structures
			   run-time-structures
			   run-time-internals-structures
			   features-structures
			   compiler-structures))


; Debug systems.

(def debug-structures
  (make-debug-structures low-structures
			 run-time-structures
			 run-time-internals-structures
			 features-structures
			 initial-structures))


; The usual development environment (scheme48.image).

(define-structure usual-structures (export (usual-features :structure))
  (open meta-module-system
	run-time-structures
	compiler-structures
	initial-structures
	(make-linker-structures features-structures
				run-time-structures
				compiler-structures))
  (files ;; more-interfaces, when not flatloading
	 more-packages))


; The linker running in a random Scheme system (Lucid, Scheme->C, or
; old version of Scheme 48).  If running in Scheme 48, this becomes
; link/linker.image.

(def alt-low-structures alt-run-time-structures
  (make-alt-run-time cheat-features-structures))

(def linker-structures
  (make-linker-structures cheat-features-structures
			  alt-run-time-structures
			  (make-compiler-structures cheat-features-structures
						    alt-run-time-structures)))



; --------------------
; Meta-interfaces.
; These are ignored by FLATLOAD, but DESIRABLE-STRUCTURES (in
; initial.scm) extracts the list of structures to be reified from
; them.

(define-interface features-structures-interface
  (export ((ascii
	    bitwise
	    code-vectors
	    features
	    records
	    signals)
	   :structure)))

; Of the features-structures, only records isn't also
; a low-structure.

(define-interface low-structures-interface
  (export ((ascii
	    bitwise
	    byte-vectors
	    cells
	    code-vectors
	    features
	    ;; records  - lose
	    signals
	    channels
	    closures
	    code-quote
	    escapes
	    locations
	    loopholes
	    low-channels
	    low-level
	    ports
	    primitives
	    scheme-level-0
	    shared-bindings
	    silly
	    source-file-names
	    structure-refs
	    debug-messages
	    vm-exposure
	    write-images)
	   :structure)))

(define-interface run-time-structures-interface
  (export ((architecture
	    channel-i/o
	    define-record-types
	    enum-case
	    enumerated
	    fluids
	    ;linked-queues
	    locks
	    queues
	    scheduler
	    scheme-level-1
	    scheme-level-2
	    templates
	    thread-cells
	    threads
	    threads-internal
	    util
	    weak)
	   :structure)))

(define-interface run-time-internals-structures-interface
  (export ((conditions
	    continuations
	    display-conditions
	    ;; escapes
	    exceptions
	    fluids-internal
	    handle
	    i/o
	    i/o-internal
	    methods
	    meta-methods
	    interrupts
	    rts-sigevents
	    rts-sigevents-internal
	    low-level
	    more-types
	    number-i/o
	    ;; primitives
	    reading
	    records-internal
	    root-scheduler
	    session-data
	    thread-cells-internal
	    usual-resumer
	    ;; silly
	    ;; structure-refs
	    ;; vm-exposure
	    wind
	    writing)
	   :structure)))
  
(define-interface compiler-structures-interface
  (export ((analysis
	    bindings
	    compiler
	    compiler-envs
	    compile-packages
	    debug-data
	    defpackage
	    filenames
	    flat-environments
	    inline
	    meta-types
	    interfaces
	    module-system
	    names
	    nodes
	    optimizer
	    packages
	    packages-internal
	    primops
	    reading-forms
	    reconstruction
	    segments
	    scan-package
	    syntactic
	    strong
	    tables
	    transforms
	    types
	    undefined
	    usual-macros)
	   :structure)))

(define-interface initial-structures-interface
  (export ((environments
	    evaluation
	    ensures-loaded
	    ;; for-reification is in there, but shouldn't get reified.
	    )
	   :structure)
	  ((make-scheme
	    make-mini-command
	    make-initial-system)
	   :procedure)))


; Initial + usual (scheme48.image).

(define-interface linker-structures-interface
  (export ((analysis
	    debuginfo
	    expander
	    flatloading
	    linker
	    link-config
	    loadc
	    reification
	    usages)
	   :structure)))

(define debug-structures-interface
  (export ((mini-eval
	    mini-environments
	    mini-scheme
	    little-system
	    mini-for-reification
	    mini-packages
	    mini-system
	    run
	    medium-scheme
	    medium-system)
	   :structure)
	  mini-eval
	  mini-environments))
