; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; More and more packages.  Some of these get loaded into the initial
; image to create scheme48.image; those that aren't can be loaded later
; using ,load-package.


; Things to load into initial.image to make scheme48.image.

(define-structure usual-features (export )  ;No exports
  (open analysis		;auto-integration
	disclosers
        command-processor
        debuginfo
        ;; Choose any combination of bignums, ratnums, recnums
	bignums ratnums recnums
	;; Choose either innums, floatnums, or neither
	innums			;Silly inexact numbers
        ;; floatnums		;Still don't print correctly
	;; pp
	;; The following is listed because this structure is used to
	;; generate a dependency list used by the Makefile...
	usual-commands))


; Command processor

(define-structures ((command-processor command-processor-interface)
		    (command (export command-processor)))
  (open scheme ;;-level-2     ; eval, interaction-environment
	tables fluids
	conditions handle
	define-record-types
	command-levels
	reading			; gobble-line, with-sharp-sharp
	i/o                     ; current-error-port
	display-conditions	; display-condition
	methods
	;; environments		; interaction-environment
	util			; unspecific
	undefined		; $note-undefined
	features		; force-output
	interrupts		; set-enabled-interrupts!, all-interrupts
	vm-exposure		; primitive-catch
	fluids-internal         ; get-dynamic-env, set-dynamic-env!
	nodes			; for ## kludge
	signals
	structure-refs
	root-scheduler)		; scheme-exit-now
  (access threads		; thread?
	  threads-internal      ; thread-continuation
	  continuations         ; continuation?
	  ports)
  (files (env version-info)
	 (env command)
	 (env read-command)))

(define-structure command-levels command-levels-interface
  (open scheme
	enumerated enum-case 
	tables
	session-data
	define-record-types
	threads threads-internal
	queues
	scheduler
	interrupts
	weak
	debug-messages		; for debugging
	signals			; error
	i/o			; current-error-port
	util                    ; unspecific
	channel-i/o             ; steal-channel-port
	fluids-internal         ; get-dynamic-env, set-dynamic-env!
	root-scheduler          ; call-when-deadlocked!
	conditions)             ; define-condition-type
  (files (env command-level)))

(define-structure basic-commands basic-commands-interface
  (open scheme-level-2
        command-processor
	command-levels
        undefined               ; noting-undefined-variables
	environments		; with-interaction-environment
	evaluation		; eval, load-into
        ;; packages		; package?
	root-scheduler		; scheme-exit-now
	)
  (files (env basic-command)))

; Usual command set
               
(define-structure usual-commands usual-commands-interface
  (open basic-commands
        build-commands
        package-commands
        debug-commands
        inspect-commands
        disassemble-commands
	;profile-commands
	))

; Image builder.

(define-structures ((build (export build-image stand-alone-resumer))
		    ;JMG added structure for scsh
		    (build-internals (export simple-condition-handler))
		    (build-commands build-commands-interface))
  (open scheme-level-2
        command-processor
	command-levels
        conditions handle
        low-level               ; flush-the-symbol-tables!
        usual-resumer
        filenames               ; translate
        display-conditions      ; display-condition
        evaluation              ; package-for-load, eval
	environments		; with-interaction-environment
	i/o			; current-error-port
        write-images
        signals)
  (files (env build)))

; Package commands.

(define-structures ((package-commands package-commands-interface)
		    (package-commands-internal
		       package-commands-internal-interface))
  (open scheme
        command-processor
	command-levels
	methods
        undefined               ; noting-undefined-variables
        packages                ; for creating a user package
        packages-internal       ; set-package-integrate?!, etc.
        package-mutation        ; package-system-sentinel
        environments            ; *structure-ref, etc.
	compiler-envs		; reflective-tower
        ensures-loaded          ; ensure-loaded
	interfaces
	ascii
	i/o			; force-output, current-error-port
        signals
	util			; every
        fluids)
  (files (env pacman)))


; Debugging aids.

; Disclosers: this makes objects and conditions print nicely.

(define-structure disclosers disclosers-interface
  (open scheme-level-1
        methods more-types
        tables
        conditions
        display-conditions
        locations
        code-vectors
        closures
        packages-internal       ; location-info-tables
        debug-data
	segments                ; get-debug-data
        enumerated              ; enumerand->name
        weak                    ; weak-pointer?
	i/o                     ; disclose-port
        templates continuations low-channels
        architecture)
  (files (env disclosers)))

; For printing procedures with their names, etc.

(define-structure debuginfo debuginfo-interface
  (open scheme-level-2
        tables
        debug-data
	segments		; debug-data-table
        ;; packages
        packages-internal	; package-name-table
        names			; generated?
	features
	weak)
  (files (env debuginfo)))

; Most of the debugging commands.

(define-structures ((debugging		;additional exports in future
		     (export breakpoint
			     continuation-parent))
		    (debug-commands debug-commands-interface))
  (open scheme-level-2
        command-processor       ; define-command, etc.
	command-levels
        fluids
        signals                 ; make-condition
        util                    ; filter
        evaluation              ; eval-from-file, eval
        environments            ; environment-define! (for ,trace)
        conditions              ; define-condition-type
        filenames               ; set-translation!
        disclosers              ; template-name, debug-data-names
        packages                ; flush-location-names, package-integrate?
        packages-internal       ; [set-]package-integrate?[!], flush-location-names
	undefined		; noting-undefined-variables
        continuations           ; continuation-template
        architecture            ; op/global, etc.
        interrupts              ; all-interrupts, set-enabled-interrupts!
        vm-exposure             ; fluid-let suppression kludge - fix later
        exceptions              ; continuation-preview
        tables
        nodes		        ; schemify for ,expand command
        reading-forms           ; $note-file-package
	debug-data segments     ;  yucko
	time                    ; real-time
        primitives              ; memory-status
        structure-refs)
  (access primitives            ; want to both use and shadow collect
          filenames             ;  and translate
	  syntactic)		;  and expand
  (files (env debug)))

; Inspector

(define-structures ((inspector (export inspect))
		    (inspector-internal (export prepare-menu))
		    (inspect-commands inspect-commands-interface))
  (open scheme-level-2
        command-processor       ; define-command, etc.
	command-levels
	define-record-types
        fluids
        debugging               ; command-loop-continuation
        closures                ; closure-template
        disclosers              ; template-debug-data, etc.
        debug-data
	evaluation		; eval
	segments                ; get-debug-data
        templates
        continuations
        names	                ; desyntaxify
        records
	records-internal
        low-level               ; vector-unassigned?
        locations
        signals                 ; error
        ;; tables  - not yet.
        weak
        util                    ; sublist
        display-conditions)     ; limited-write
  (files (env inspect)))

(define-structure list-interfaces (export list-interface)
  (open scheme-level-2 interfaces packages meta-types sort bindings)
  (files (env list-interface)))


; Package and interface mutation.

(define-structure package-mutation package-mutation-interface
  (open scheme-level-2
        shadowing               ; shadow-location!
        packages
        interfaces
	bindings
        packages-internal
        defpackage              ; set-verify-later!
        locations
        disclosers              ; location-info
        handle
        tables fluids weak signals)
  (files (env pedit)))

; The following hooks the compiler up with an exception handler for
; unbound variables.

(define-structure shadowing (export shadow-location!)
  (open scheme-level-1
        vm-exposure             ;primitive-catch
        continuations templates locations code-vectors
        exceptions signals
        architecture)   ;(enum op global)
  (files (env shadow)))     ;Exception handler to support package system


; Disassembler

(define-structures ((disassembler
		       (export disassemble write-instruction))
		    (disassemble-commands disassemble-commands-interface))
  (open scheme-level-2
        command-processor       ; define-command
	disclosers              ; template-name
        enumerated              ; enumerand->name
        disclosers              ; location-name
	evaluation		; eval
        templates
        continuations
        locations
        code-vectors
        closures
        architecture
        signals)
  (files (env disasm)))

; Assembler.

(define-structure assembling (export)	; No exports, this defines a compilator.
  (open scheme-level-2
	compiler		;define-compilator
	segments
	architecture
	nodes			;node-form node-ref
	bindings		;binding? binding-place
        meta-types              ;value-type
        templates               ; for Richard's version
        signals                 ;error
        enumerated              ;name->enumerand
        code-vectors)
  (files (env assem)))

; Foo

(define-structure assembler (export (lap :syntax))
  (open scheme-level-2)
  (for-syntax (open scheme-level-2 nodes meta-types assembling))
  (begin
    (define-syntax lap
      (lambda (e r c)
        (make-node (get-operator 'lap syntax-type) e)))))

; Execution profiler.

(define-structures ((profile (export run-with-profiling))
		    (profile-commands profile-commands-interface))
  (open scheme
	command-processor
        continuations
        architecture
        interrupts
        tables
        primitives     ; schedule-interrupt
        wind
        disclosers
	time
        sort
        escapes)       ; primitive-cwcc
  (files (env profile)))

; Large integers and rational and complex numbers.

(define-structure extended-numbers extended-numbers-interface
  (open scheme-level-2
        methods meta-methods
        define-record-types
        exceptions              ; make-opcode-generic!
        primitives
        architecture
        signals
	util
        number-i/o)
  (files (rts xnum)))

(define-structure bignums bignums-interface
  (open scheme-level-2
        extended-numbers
        methods signals)
  (files (rts bignum))
  (optimize auto-integrate))

(define-structure innums (export )    ;inexact numbers
  (open scheme-level-2
        extended-numbers
        methods signals
        number-i/o)             ;string->integer
  (files (rts innum)))

(define-structure ratnums (export )    ;No exports
  (open scheme-level-2
        extended-numbers
        methods signals
        number-i/o)             ;string->integer
  (files (rts ratnum)))

(define-structure recnums (export )    ;No exports
  (open scheme-level-2
        extended-numbers
        methods signals
        number-i/o)             ;really-number->string
  (files (rts recnum)))

(define-structure floatnums
		  (export floatnum? exp log sin cos tan asin acos atan sqrt)
  (open scheme-level-2
        extended-numbers
        code-vectors
        methods signals
	enumerated
	loopholes
	more-types		;:double
        primitives)             ;vm-extension double?
  (files (rts floatnum))
  (optimize auto-integrate))

(define-structure time time-interface
  (open scheme-level-1 primitives architecture enumerated)
  (files (rts time)))

(define-structure placeholders placeholder-interface
  (open scheme-level-1 define-record-types
	threads threads-internal queues
	interrupts
	signals)
  (files (big placeholder))
  (optimize auto-integrate))


;----------------
; Big Scheme

(define-structure random (export make-random)
  (open scheme-level-2 bitwise
	signals)		;call-error
  (files (big random)))

(define-structure pp (export p pretty-print define-indentation)
  (open scheme-level-2
        tables
        methods)               ;disclose
  (files (big pp)))


; Bitwise logical operators on bignums.

(define-structure bigbit (export )  ;No exports
  (open scheme-level-2
	bignums
	methods
        extended-numbers
        ;; exceptions
	;; architecture
        bitwise
	signals)
  (files (big bigbit)))

(define-structure formats (export format)
  (open scheme-level-2 ascii signals
	extended-ports)
  (files (big format)))

(define-structure extended-ports extended-ports-interface
  (open scheme-level-2 records define-record-types ports i/o i/o-internal
	code-vectors ascii
	structure-refs
	signals)
  (access primitives)     ; copy-bytes!
  (files (big more-port)))

(define-structure destructuring (export (destructure :syntax))
  (open scheme-level-2)
  (files (big destructure)))

(define-structure arrays arrays-interface
  (open scheme-level-2 define-record-types signals)
  (files (big array)))

(define-structure receiving (export (receive :syntax))
  (open scheme-level-2)
  (files (big receive)))

(define-structure defrecord defrecord-interface
  (open scheme-level-1 records loopholes
	primitives) ; unspecific
  (files (big defrecord)))

(define-structures ((enum-sets enum-sets-interface)
		    (enum-sets-internal enum-sets-internal-interface))
  (open scheme define-record-types
	finite-types
	bitwise 
	util
	signals
	external-calls)
  (optimize auto-integrate)
  (files (big enum-set)))

(define general-tables tables)    ; backward compatibility

(define-structure thread-fluids thread-fluids-interface
  (open scheme define-record-types weak
	threads thread-cells fluids)
  (files (big thread-fluid)))

;;; Package defs for the Scheme Underground sorting package,

;;; The general sort package:

(define-structure sorting sorting-interface
  (open scheme
	list-merge-sort
	vector-heap-sort
	vector-merge-sort
	sorted
	delete-neighbor-duplicates)
  (files (sort sort))
  (optimize auto-integrate))

(define-structure sorted sorted-interface
  (open scheme
	vector-utils)
  (files (sort sortp))
  (optimize auto-integrate))

(define-structure delete-neighbor-duplicates delete-neighbor-duplicates-interface
  (open scheme
	receiving
	vector-utils)
  (files (sort delndups))
  (optimize auto-integrate))

(define-structure binary-searches binary-searches-interface
  (open scheme
	vector-utils)
  (files (sort vbinsearch)))

(define-structure list-merge-sort list-merge-sort-interface
  (open scheme
	receiving
	(subset signals (error)))
  (files (sort lmsort))
  (optimize auto-integrate))

(define-structure vector-merge-sort vector-merge-sort-interface
  (open scheme
	receiving
	vector-utils
	vector-insertion-sort-internal)
  (files (sort vmsort))
  (optimize auto-integrate))

(define-structure vector-heap-sort vector-heap-sort-interface
  (open scheme
	receiving
	vector-utils)
  (files (sort vhsort))
  (optimize auto-integrate))

(define-structures ((vector-insertion-sort vector-insertion-sort-interface)
		    (vector-insertion-sort-internal
		     vector-insertion-sort-internal-interface))
  (open scheme
	vector-utils)
  (files (sort visort))
  (optimize auto-integrate))

(define-structure vector-utils (export vector-copy
				       vector-portion-copy
				       vector-portion-copy!
				       vector-start+end
				       vectors-start+end-2)
  (open scheme)
  (files (sort vector-util)))

;;; end Package defs for the Scheme Underground sorting package,

(define-structure sort (export sort-list sort-list!)
  (open scheme-level-2
	sorting)
  (begin
    (define (sort-list l obj-<)
      (list-sort obj-< l))
    (define (sort-list! l obj-<)
      (list-sort! obj-< l))))

(define-structure big-util big-util-interface
  (open scheme-level-2
	formats
	features		; immutable? make-immutable!
        structure-refs)         ; structure-ref
  (access signals               ; error
          debugging		; breakpoint
	  primitives)		; copy-bytes!
  (files (big big-util)))

(define-structure big-scheme big-scheme-interface
  (open scheme-level-2
	formats
	sort
        extended-ports
	pp
	enumerated
        bitwise
        ascii
        bigbit
	big-util
        tables
	;defrecord
        destructuring
        receiving))

; Things needed for connecting with external code.

(define-structure external-calls (export call-imported-binding
					 lookup-imported-binding
					 define-exported-binding
					 shared-binding-ref
					 ((import-definition
					   import-lambda-definition)
					  :syntax)
					 add-finalizer!
					 define-record-resumer
					 call-external-value)
  (open scheme-level-2 define-record-types
	primitives
        architecture
	exceptions interrupts signals
	placeholders
	shared-bindings
	byte-vectors
	bitwise bigbit		;for {enter|extract}_integer() helpers
	records)
  (files (big import-def)
	 (big callback)))

(define-structure dynamic-externals dynamic-externals-interface
  (open scheme-level-2 define-record-types tables
        signals                 ;warn
	primitives		;find-all-records
	i/o			;current-error-port
        code-vectors
	external-calls)
  (files (big external)))

; Externals - this is obsolete; use external-calls and dynamic-externals
; instead.

(define-structure externals (compound-interface
			       dynamic-externals-interface
			       (export external-call
				       null-terminate))
  (open scheme-level-2 structure-refs
	dynamic-externals)
  (access external-calls)
  (begin
   ; We fake the old external-call primitive using the new one and a
   ; a C helper procedure from c/unix/dynamo.c.

    (define (external-call proc . args)
      (let ((args (apply vector args)))
	(old-external-call (external-value proc) args)))
    
    ((structure-ref external-calls import-lambda-definition)
       old-external-call (proc args)
       "s48_old_external_call")

    ; All strings are now null terminated.
    (define (null-terminate string) string)))

; Rudimentary object dump and restore

(define-structure dump/restore dump/restore-interface
  (open scheme-level-1
        number-i/o
        tables
        records
        signals                 ;error
        locations               ;make-undefined-location
        closures
        code-vectors            ;code vectors
        fluids
        ascii
        bitwise
        methods                 ;disclose
        templates)              ;template-info
  (files (big dump)))

; Unix Sockets

(define-structure sockets
    (export open-socket
	    close-socket
	    socket-accept
	    socket-port-number
	    socket-client

	    get-host-name

	    ; From the old interface; I would like to get rid of these.
	    socket-listen
	    socket-listen-channels
	    socket-client-channels
	    )
  (open scheme define-record-types
	external-calls
	low-channels	; channel? close-channel
	signals		; error
	interrupts	; enable-interrupts! disable-interrupts!
	channel-i/o)	; wait-for-channel {in|out}put-channel->port
  (files (big socket)))

; Heap traverser

(define-structure traverse
                  (export traverse-depth-first traverse-breadth-first trail
                          set-leaf-predicate! usual-leaf-predicate)
  (open scheme-level-2
        primitives              ; ?
        queues tables
        bitwise locations closures code-vectors
        disclosers              ; foo
        features                ; string-hash
        low-level)              ; flush-the-symbol-table!, vector-unassigned?
  (files (env traverse)))

; Space analyzer

(define-structure spatial (export space vector-space record-space)
  (open scheme
	architecture primitives assembler packages enumerated 
	features sort locations display-conditions)
  (files (env space)))

; Structure & Interpretation compatibility

(define-structure sicp sicp-interface
  (open scheme-level-2 signals tables)
  (files (misc sicp)))

; red-black balanced binary search trees

(define-structure search-trees search-trees-interface
  (open scheme-level-2 define-record-types)
  (optimize auto-integrate)
  (files (big search-tree)))

(define-structure finite-types (export ((define-finite-type
					 define-enumerated-type) :syntax))
  (open scheme-level-2 code-quote define-record-types
	enumerated
	features)		; make-immutable
  (files (big finite-type)))

; nondeterminism via call/cc

(define-structure nondeterminism (export with-nondeterminism
					 ((either one-value all-values) :syntax)
					 fail)
  (open scheme-level-2
	fluids cells
	(subset signals (error)))
  (files (big either)))

;----------------
; SRFI packages

; SRFI-0 - Doesn't work with the module system.

; Olin's list library.

(define-structure srfi-1 srfi-1-interface
  (open scheme-level-2
	receiving
	(subset signals (error)))
  (files (srfi srfi-1)))

(define-structure srfi-2 (export (and-let* :syntax))
  (open scheme-level-2
	signals)		; error
  (files (srfi srfi-2)))

; SRFI-3 - withdrawn
; SRFI-4 - needs hacks to the reader

(define-structure srfi-5 (export (let :syntax))
  (open (modify scheme-level-2 (hide let)))
  (files (srfi srfi-5)))

(define-structure srfi-6 (export open-input-string
				 open-output-string
				 get-output-string)
  (open (modify extended-ports
		(rename (make-string-input-port    open-input-string)
			(make-string-output-port   open-output-string)
			(string-output-port-output get-output-string)))))

; Configuration language

(define-structure srfi-7 (export)	; defines a command
  (open scheme

	; for parsing programs
	receiving
	nondeterminism
	(subset signals (error))

	(subset package-commands-internal	(config-package))
	ensures-loaded
	(subset packages			(note-structure-name!))

	; for defining the command
	(subset command-processor		(define-user-command-syntax
						 user-command-environment))
	(subset environments			(environment-define!)))

  (begin
    (define available-srfis
      '(srfi-1 srfi-2 srfi-5 srfi-6 srfi-7 srfi-8 srfi-9
	srfi-11 srfi-13 srfi-14 srfi-16 srfi-17 srfi-19 srfi-23
	srfi-25 srfi-26 srfi-27 srfi-28 
        srfi-30 srfi-31	srfi-37
        srfi-42))

    ; Some SRFI's redefine Scheme variables.
    (define shadowed
      '((srfi-1 map for-each member assoc)
	(srfi-5 let)
	(srfi-13 string->list string-copy string-fill!)
	(srfi-17 set!)))
    )

  (files (srfi srfi-7)))

; Taken directly from the SRFI document (or from `receiving', take your pick).

(define-structure srfi-8 (export (receive :syntax))
  (open scheme-level-2)
  (begin
    (define-syntax receive
      (syntax-rules ()
	((receive formals expression body ...)
	 (call-with-values (lambda () expression)
			   (lambda formals body ...)))))))

; SRFI-9 is a slight modification of DEFINE-RECORD-TYPE.

(define-structure srfi-9 (export (define-record-type :syntax))
  (open scheme-level-2 
	(with-prefix define-record-types sys:))
  (begin
    (define-syntax define-record-type
      (syntax-rules ()
	((define-record-type type-name . stuff)
	 (sys:define-record-type type-name type-name . stuff))))))

; SRFI-10 - no stand-alone interface.

(define-structure srfi-11 (export (let-values :syntax)
				  (let*-values :syntax))
  (open scheme-level-2)
  (files (srfi srfi-11)))

; SRFI-12 - withdrawn

; Two more encyclopedias from Olin.

(define-structure srfi-13 srfi-13-interface
  (open scheme-level-2
	bitwise
	srfi-8 srfi-14
	(subset signals		(error)))
  (files (srfi srfi-13)))

(define-structure srfi-14 srfi-14-interface
  (open scheme-level-2
	bitwise
	srfi-9
	(modify ascii (rename (char->ascii %char->latin1)
			      (ascii->char %latin1->char)))
	(subset features (make-immutable!))
	(subset signals (error)))
  (files (srfi srfi-14)))

; SRFI-15 - withdrawn

(define-structure srfi-16 (export (case-lambda :syntax))
  (open scheme-level-2
	(subset signals (error)))
  (files (srfi srfi-16)))

(define-structure srfi-17 (export (set! :syntax) setter)
  (open (modify scheme-level-2 (rename (set! scheme-set!)))
	(subset signals (error))
	(subset util (unspecific)))
  (files (srfi srfi-17)))

; SRFI-18 - no implementation given

(define (make-srfi-19 scheme-with-scsh)
  (structure srfi-19-interface
     (open scheme-with-scsh
           (subset srfi-1 (reverse!))
           srfi-6
           srfi-8
           signals
           srfi-9)
     (files (srfi srfi-19))))

; SRFI-20 - withdrawn
; SRFI-21 - no implementation given
; SRFI-22 - no implementation given

(define-structure srfi-23 (export error)
  (open (subset signals (error))))

; SRFI-24 - withdrawn

(define-structure srfi-25 (export 
			   array? make-array shape array 
			   array-rank 
			   array-start array-end 
			   array-ref array-set! share-array)
  (open scheme
	srfi-23
	srfi-9)
  (files (srfi srfi-25)))
  

(define-structure srfi-26 (export ((cut cute) :syntax))
  (open scheme)
  (files (srfi srfi-26)))

(define-structure srfi-27 srfi-27-interface
  (open
   scheme-level-1
   floatnums
   external-calls
   (subset srfi-9 (define-record-type)) 
   (subset srfi-23 (error)))
;; scsh doesn't have S48's posix subsystem yet:
;   (subset posix-time (current-time)) 
;   (subset posix (time-seconds)))
  (files (srfi srfi-27)))

(define-structure srfi-28 (export format)
  (open scheme
	srfi-23
	srfi-6)
  (files (srfi srfi-28)))

; SRFI-29 - requires access to the current locale

; SRFI-30 - scheme/rts/read.scm contains the reader for #|...|# comments

(define-structure srfi-31 srfi-31-interface
  (open scheme)
  (files (srfi srfi-31)))

(define-structure srfi-37 srfi-37-interface
  (open scheme 
	srfi-9 
	srfi-11)
  (files (srfi srfi-37)))

; Eager Comprehensions

(define-structure srfi-42 srfi-42-interface
  (open scheme
	srfi-23)
  (files (srfi srfi-42)))
; ... end of package definitions.

; Temporary compatibility stuff
(define-syntax define-signature
  (syntax-rules () ((define-signature . ?rest) (define-interface . ?rest))))
(define-syntax define-package
  (syntax-rules () ((define-package . ?rest) (define-structures . ?rest))))
(define table tables)
(define record records)


; Must list all the packages defined in this file that are to be
; visible in the command processor's config package.

(define-interface more-structures-interface
  (export ((more-structures
	    usual-features
	    arrays
	    assembler
	    assembling
	    general-tables
	    bigbit
	    bignums ratnums recnums floatnums
	    build
	    callback
	    cells
	    command-levels
	    command-processor
	    debugging
	    define-record-types
	    defrecord
	    destructuring
	    disassembler
	    disclosers
	    dump/restore
	    dynamic-externals
	    enum-case
	    enum-sets
	    extended-numbers
	    extended-ports
	    externals
	    external-calls
	    finite-types
	    formats
	    innums
	    inspector
	    inspector-internal
	    ;linked-queues
	    list-interfaces
	    ;more-threads
	    package-commands-internal
	    package-mutation
	    placeholders
	    pp
	    ;profile
	    queues
	    time
	    random
	    receiving
	    search-trees
	    sicp
	    sockets

	    sort
	    delete-neighbor-duplicates
	    binary-searches
	    sorted
	    list-merge-sort
	    vector-merge-sort
	    vector-heap-sort
	    vector-insertion-sort
	    sorting

	    strong
	    thread-fluids
	    traverse
	    spatial
	    big-scheme
	    big-util
	    ;; From link-packages.scm:
	    analysis
	    debuginfo
	    expander
	    flatloading
	    linker
	    link-config
	    reification			;?
	    shadowing
	    ;; Compatibility
	    record table
	    build-internals                  ;added by JMG

            ; SRFI packages
	    srfi-1 srfi-2 srfi-5 srfi-6 srfi-7 srfi-8 srfi-9
	    srfi-11 srfi-13 srfi-14 srfi-16 srfi-17
	    srfi-23 srfi-25 srfi-26 srfi-27 srfi-28
	    srfi-31 srfi-37
            srfi-42
	    )
	   :structure)
          make-srfi-19
	  ((define-signature define-package) :syntax)))
