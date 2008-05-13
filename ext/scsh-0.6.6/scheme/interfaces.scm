; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; Some interfaces.  Order of presentation is a bit random.

(define-interface scheme-level-0-interface
  (export ((if begin lambda letrec quote set!
	       define define-syntax let-syntax letrec-syntax)
	   :syntax)
	  
	  ;; The basic derived expression types.
	  ((and cond do let let* or) :syntax)

	  apply

	  ;; Scalar primitives
	  eq?
	  number? integer? rational? real? complex?
	  exact? exact->inexact inexact->exact
	  + - * / = < > <= >=
	  quotient remainder
	  floor numerator denominator
	  real-part imag-part
	  exp log sin cos tan asin acos atan sqrt
	  angle magnitude make-polar make-rectangular
	  char?
	  char=? char<?
	  eof-object?
	  input-port? output-port?

	  ;; Data manipulation
	  pair? cons car cdr set-car! set-cdr!
	  symbol? symbol->string
	  string? make-string string-length string-ref string-set!
	  vector? make-vector vector-length vector-ref vector-set!

	  ;; Unnecessarily primitive
	  read-char peek-char write-char
	  string=?
	  vector
	  assq

	  ;; New in Revised^5 Scheme
	  values call-with-values

	  ;; Things aren't primitive at the VM level, but they have
	  ;; to be defined using VM primitives.
	  string-copy
	  string->symbol
	  procedure?
	  integer->char char->integer))

; Miscellaneous primitives.

(define-interface primitives-interface
  (export add-finalizer!
	  add-pending-channel
	  call-external-value
	  checked-record-ref
	  checked-record-set!
	  collect			;,collect command
	  continuation-length
	  continuation-ref
	  continuation-set!
	  continuation?
	  copy-bytes!
	  current-thread		;fluids threads
	  double?
	  eof-object                    ;i/o-internal re-exports this
	  extended-number-length
	  extended-number-ref
	  extended-number-set!
	  extended-number?
	  find-all			;externals, command-levels
	  find-all-records		;threads
	  immutable?
	  make-continuation
	  make-double
	  make-extended-number
	  make-immutable!
	  make-record
	  make-template
	  make-weak-pointer
	  memory-status			;interrupts
	  record
	  record-length
	  record-ref
	  record-set!
	  record?
	  return-from-callback
	  schedule-interrupt            ;interrupts re-exports this
	  session-data			;channels
	  set-current-thread!		;fluids threads
	  set-enabled-interrupts!	;interrupts
	  set-exception-handlers!
	  set-interrupt-handlers!	;interrupts
	  set-session-data!		;channels
	  string-hash
	  template-length
	  template-ref
	  template-set!
	  template?
	  time
	  unspecific			;record
	  vm-extension
	  wait
	  weak-pointer-ref
	  weak-pointer?))

(define-interface bitwise-interface
  (export arithmetic-shift
	  bitwise-not
	  ((bitwise-and bitwise-ior bitwise-xor)
	   (proc (&rest :exact-integer) :exact-integer))))

(define-interface cells-interface
  (export cell?
	  make-cell
	  cell-ref
	  cell-set!))

(define-interface locations-interface
  (export location?
	  location-defined?
	  location-assigned?
	  location-id
	  set-location-id!
	  make-undefined-location
	  set-location-defined?!
	  contents
	  set-contents!))

(define-interface closures-interface
  (export closure?
	  make-closure
	  closure-env
	  closure-template))

(define-interface byte-vectors-interface
  (export byte-vector?
	  byte-vector-length
	  byte-vector-ref
	  byte-vector-set!
	  make-byte-vector
	  
	  byte-vector))

; Same again, but with old names for compatibility.

(define-interface code-vectors-interface
  (export code-vector?
	  code-vector-length
	  code-vector-ref
	  code-vector-set!
	  make-code-vector))

(define-interface channels-interface
  (export input-channel?
	  open-input-channel
	  channel-read	  
	  close-input-channel
	  
	  output-channel?
	  open-output-channel
	  channel-write
	  close-output-channel

	  set-with-fs-context-aligned*!))

(define-interface low-channels-interface
  (export channel?
	  channel-id
	  channel-status
	  channel-os-index
	  
	  channel-ready?
	  
	  open-channel
	  close-channel
	  channel-read
	  channel-write
	  channel-abort

	  open-channels-list))

(define-interface ports-interface
  (export port?
	  make-port
	  port-handler set-port-handler!
	  port-lock set-port-lock!
	  port-locked? set-port-locked?!
	  port-status set-port-status!
	  port-data set-port-data!
	  port-buffer set-port-buffer!
	  port-index set-port-index!
	  port-limit set-port-limit!
	  port-pending-eof? set-port-pending-eof?!))

(define-interface locks-interface
  (export lock?
	  make-lock
	  obtain-lock
	  obtain-lock-multiple
	  maybe-obtain-lock
	  release-lock
	  lock-owner-uid))    ;really should be internal

(define-interface shared-bindings-interface
  (export shared-binding?
	  shared-binding-is-import?
	  shared-binding-name
	  shared-binding-ref
	  shared-binding-set!	  
	  lookup-imported-binding
	  define-imported-binding
	  undefine-imported-binding
	  lookup-exported-binding
	  define-exported-binding
	  undefine-exported-binding))

; Miscellaneous useful procedures used in various random places.
; These are used in the byte-code compiler and the linker and so have
; alternative implementations in alt/...

(define-interface features-interface
  (export immutable?
	  make-immutable!
	  string-hash
	  force-output
	  current-noise-port))

; Another hodge-podge.

(define-interface low-level-interface
  (export vector-unassigned?))		;inspector

(define-interface vm-exposure-interface
  (export invoke-closure		;eval
	  primitive-catch))		;shadowing, start, command

(define-interface escapes-interface
  (export primitive-cwcc
	  with-continuation))

(define-interface ascii-interface
  (export ascii->char
	  char->ascii
	  ascii-limit
	  ascii-whitespaces))

; Level 1: The rest of Scheme except for I/O.

(define-interface scheme-level-1-adds-interface
  (export ((case delay quasiquote syntax-rules) :syntax)
	  abs
	  append  assoc assv	  
	  boolean?
	  caaaar caaadr caadar caaddr caaar caadr caar
	  cadaar cadadr caddar cadddr cadar caddr cadr
	  cdaaar cdaadr cdadar cdaddr cdaar cdadr cdar
	  cddaar cddadr cdddar cddddr cddar cdddr cddr
	  char->integer char-alphabetic?
	  ceiling
	  char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
	  char-downcase char-lower-case? char-numeric?
	  char-upcase
	  char-upper-case? char-whitespace? char<=?
	  char>=? char>?
	  equal? eqv? even? expt
	  for-each force
	  gcd
	  inexact?
	  integer->char
	  lcm length list list->string list->vector
	  list?				;New in R4RS
	  list-ref list-tail
	  map max member memq memv min modulo
	  negative? not null?
	  odd?
	  positive?
	  procedure?			;from low-level
	  rationalize
	  reverse
	  round 
	  string string->list string->symbol
	  string-append
	  string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
	  string-copy			;from low-level
	  string-fill!
	  string<=? string<? string=? string>=? string>?
	  substring
	  truncate
	  vector->list vector-fill!
	  zero?))

(define-interface scheme-level-1-interface
  (compound-interface scheme-level-1-adds-interface
		      scheme-level-0-interface))

(define-interface util-interface
  (export any
	  every
	  filter
	  fold fold->2 fold->3
	  last
	  posq
	  posv
	  position
	  reduce
	  sublist
	  insert
	  unspecific
	  
	  (mvlet :syntax)))

; Level 2 consists of harder things built on level 1.

(define-interface records-interface
  (export make-record-type
	  record-constructor
	  record-accessor
	  record-modifier
	  record-predicate
	  define-record-discloser
	  define-record-resumer
	  record-type?
	  record-type-field-names
	  record-type-name))

(define-interface records-internal-interface
  (export record
	  record?
	  record-type
	  record-length
	  record-ref
	  record-set!
	  :record-type
	  disclose-record
	  initialize-records!))

(define-interface define-record-types-interface
  (export (define-record-type :syntax)
	  define-record-discloser))

(define-interface methods-interface
  (export (define-generic :syntax)
	  (define-method :syntax)
	  (define-simple-type :syntax)
	  :values
	  :value
	  :number
	  :complex
	  :real
	  :rational
	  :integer
	  :exact-integer
	  :boolean
	  :symbol
	  :char
	  :null
	  :pair
	  :vector
	  :string
	  :procedure
	  :input-port
	  :output-port
	  :eof-object
	  :record
	  :record-type
	  :zero
	  singleton
	  disclose &disclose))

(define-interface meta-methods-interface
  (export type-predicate &type-predicate
	  type-priority  &type-priority
	  type-superiors &type-superiors
	  same-type?	 &same-type?
	  add-method!	 &add-method!
	  set-final-method!
	  make-method-table         ;rts/exception.scm
	  method-table-get-perform  ;rts/exception.scm
	  make-simple-type
	  ;; Hooks for DOODL or other future extension
	  :simple-type :method-info :method-table :singleton
	  make-generic
	  (method-info :syntax)
	  methods->perform))

(define-interface number-i/o-interface
  (export number->string &number->string
	  string->number
	  really-string->number &really-string->number ;exactness argument
	  string->integer))

(define-interface fluids-interface
  (export make-fluid
	  let-fluid
	  let-fluids
	  fluid
	  
	  fluid-cell-ref
	  fluid-cell-set!

	  set-fluid!))

(define-interface fluids-internal-interface
  (export initialize-dynamic-state!
	  current-thread
	  set-current-thread!
	  get-dynamic-env		; wind.scm, env/command-level.scm
	  set-dynamic-env!		; wind.scm
	  get-dynamic-point		; wind.scm
	  set-dynamic-point!))		; wind.scm

(define-interface thread-cells-internal-interface
  (export get-thread-cell-env
	  set-thread-cell-env!
	  empty-thread-cell-env))

(define-interface thread-cells-interface
  (export make-thread-cell
	  thread-cell-ref
	  thread-cell-set!))

(define-interface enumerated-interface
  (export (define-enumeration :syntax)
	  (enum :syntax)
	  (enumerand->name :syntax)
	  (name->enumerand :syntax)))

(define :enumeration :syntax)

(define-interface signals-interface
  (export error warn syntax-error call-error
	  signal signal-condition
	  make-condition))

(define-interface handle-interface
  (export ignore-errors report-errors-as-warnings with-handler))

(define-interface conditions-interface
  (export define-condition-type
	  condition-predicate
	  condition-stuff condition-type
	  error? warning? syntax-error? call-error? read-error?
	  interrupt?

	  ;; Do these belong here?... not really.
	  exception-arguments
	  exception-reason
	  exception-opcode
	  exception?
	  make-exception))

(define-interface wind-interface
  (export call-with-current-continuation
	  dynamic-wind))

(define-interface i/o-interface
  (export current-input-port current-output-port
	  close-output-port close-input-port

	  ;; Added for scsh -dfisher and JMG;;
	  $current-input-port
	  $current-output-port
	  $current-error-port
	  $current-noise-port
	  make-buffered-input-port-handler
	  make-buffered-output-port-handler
	  make-buffered-input-port
	  make-buffered-output-port
	  ;; end of additions. ;;
	  char-ready?
	  read-block write-block
	  newline
	  input-port-option		;read.scm
	  output-port-option		;write.scm
	  write-string			;write.scm
	  initialize-i/o                ;init.scm
	  initialize-output-port-list!  ;init.scm
	  with-current-ports
	  initialize-i/o-handlers!      ;init.scm
	  disclose-port
	  current-error-port
	  current-noise-port
	  force-output			;xport.scm
	  output-port-ready?
	  input-port?
	  output-port?
	  silently
	  make-null-output-port))

(define-interface i/o-internal-interface
  (export make-input-port
	  make-output-port make-unbuffered-output-port
	  make-port-handler
	  port-handler-buffer-proc     ; extended-ports
	  port-handler-discloser
	  port-handler-close
	  port-handler-ready?
	  port-handler-steal
	  
	  default-buffer-size

	  open-input-port?
	  open-input-port-status
	  make-input-port-closed!
	  open-output-port?
	  open-output-port-status
	  make-output-port-closed!

	  eof-object                   ; users will probably want this

	  obtain-port-lock
	  release-port-lock

	  periodically-force-output!
	  periodically-flushed-ports
	  force-output-if-open
	  really-force-output          ; assumes port alread locked

	  output-port-forcers	       ;root-scheduler.scm
	  
  	  steal-port!                  ; JMG: for scsh
	  fill-port-buffer!            ; JMG: for read-delimited
	  call-with-current-input-port
	  call-with-current-output-port
	  call-with-current-noise-port))

(define-interface channel-i/o-interface
  (export call-with-input-file call-with-output-file
	  with-input-from-file with-output-to-file
	  open-input-file open-output-file

	  initialize-channel-i/o!       ;scheduler
	  waiting-for-i/o?              ;scheduler

	  steal-channel!                ;JMG: 3 For scsh.
	  steal-port! 
	  channel-cell-ref

	  input-channel->port           ;usual-resumer posix
	  output-channel->port          ;usual-resumer posix

	  input-channel+closer->port    ;big/socket.scm
	  output-channel+closer->port	;big/socket.scm
	  
	  ; call WAIT-FOR-CHANNEL with interrupts disabled
	  wait-for-channels
	  wait-for-channel		;big/socket.scm

	  port->channel			;posix
	  force-channel-output-ports!	;posix
	  ))

(define-interface threads-interface
  (export thread?
	  thread-name thread-uid        ;for debugging

	  spawn narrow
	  relinquish-timeslice
	  sleep
	  terminate-current-thread))

(define-interface threads-internal-interface
  (export make-thread
	  thread-data set-thread-data!
	  thread-continuation           ;debugger
	  thread-uid->thread            ;expensive!
	  thread-scheduler set-thread-scheduler!
	  set-thread-arguments!

	  current-thread

	  enqueue-thread!
	  thread-queue-empty?
	  maybe-dequeue-thread!
	  block-on-queue

	  event-pending?
	  get-next-event!
	  schedule-event
	  (event-type :syntax)

	  run
	  running?
	  
	  block
	  make-ready
	  set-thread-cell! clear-thread-cell!
	  spawn-on-scheduler spawn-on-root
	  wait
	  upcall propogate-upcall
	  interrupt-thread
	  kill-thread!
	  terminate-thread!

	  wake-some-threads
	  register-dozer

	  all-threads                   ; for command-levels

	  with-threads))		; rts/init.scm

(define-interface scheduler-interface
  (export run-threads
	  run-threads-with-housekeeper
	  round-robin-event-handler
	  handle-narrow-event
 
	  make-counter            ; for thread counts
	  counter-value
          increment-counter!
          decrement-counter!))

(define-interface queues-interface
  (export make-queue enqueue! dequeue! maybe-dequeue! queue-empty?
	  queue? queue->list queue-front queue-length
	  delete-from-queue! on-queue?))

(define-interface exceptions-interface
  (export define-exception-handler
	  initialize-exceptions!
	  extend-opcode!
	  signal-exception
	  continuation-preview))	;env/debug.scm

(define-interface interrupts-interface
  (export initialize-interrupts!	;init.scm
	  all-interrupts
	  no-interrupts
	  ;reset-timer-interrupts!
	  set-interrupt-handler!
	  interrupt?
	  ;set-timer-interrupt!
	  schedule-interrupt
	  set-enabled-interrupts!	;command.scm
	  enabled-interrupts
	  enable-interrupts!
	  disable-interrupts!
	  with-interrupts-allowed
	  with-interrupts-inhibited
	  call-after-gc!
	  call-before-heap-overflow!
	  (interrupt :syntax)))

(define-interface rts-sigevents-interface
  (export rts-next-sigevent
	  rts-next-sigevent/no-wait
	  most-recent-sigevent
	  sigevent?
	  sigevent-type
	  schedule-timer-interrupt!))

(define-interface rts-sigevents-internal-interface
  (export waiting-for-sigevent?
	  with-sigevents))

(define-interface writing-interface
  (export write
	  display
	  display-type-name
	  recurring-write))

(define-interface reading-interface
  (export read
	  define-sharp-macro		;command.scm
	  reading-error
	  gobble-line))

; Level 2: the harder stuff.
; Various primitives get numeric types and promoted to n-ary at this point.

(define-interface scheme-level-2-adds-interface
  (export (char-ready? (proc (&opt :input-port) :boolean))
	  call-with-current-continuation
	  call-with-input-file call-with-output-file
	  current-input-port current-output-port
	  dynamic-wind			;New in R5RS
	  close-input-port close-output-port
	  open-input-file open-output-file
	  with-input-from-file with-output-to-file
	  number->string string->number
	  newline display write
	  read))

(define-interface scheme-level-2-interface
  (compound-interface scheme-level-2-adds-interface
		      scheme-level-1-interface))

(define-interface scheme-adds-interface
  (export eval load
	  interaction-environment
	  scheme-report-environment
	  null-environment))

(define-interface scheme-interface
  (compound-interface scheme-adds-interface
		      scheme-level-2-interface))

(define-interface continuations-interface
  (export continuation-arg
	  continuation-arg-count
	  continuation-cont
	  continuation-env
	  continuation-pc
	  continuation-template
	  continuation-parent
	  continuation?
	  :continuation

	  exception-continuation?
	  exception-continuation-exception))

(define-interface templates-interface
  (export make-template
	  template-code
	  template-info
	  template-length
	  template-ref
	  template-overhead
	  set-template-code!
	  set-template-info!
	  template-set!
	  template?))

(define-interface weak-interface
  (export weak-pointer?			;disclosers.scm
	  weak-pointer-ref
	  make-weak-pointer
	  make-population
	  add-to-population!
	  population->list
	  walk-population))

(define-interface filenames-interface
  (export namestring *scheme-file-type* *load-file-type*
	  file-name-directory
	  file-name-nondirectory
	  translate
	  set-translation!
	  translations))


; Things for the compiler.

(define-interface general-tables-interface
  (export make-table
	  make-string-table
	  make-symbol-table
	  make-integer-table
	  make-table-maker
	  table?
	  table-ref
	  table-set!
	  table-walk
	  make-table-immutable!

	  string-hash
	  default-table-hash-function))

;----------------
; Beginning of compiler interfaces

(define-interface usual-macros-interface
  (export usual-transform))

(define-interface meta-types-interface
  (export same-type?
	  subtype?
	  meet?
	  join-type
	  meet-type
	  sexp->type type->sexp rail-type->sexp
	  syntax-type
	  any-values-type
	  any-arguments-type
	  value-type
	  error-type

	  make-some-values-type

	  empty-rail-type
	  rail-type
	  make-optional-type
	  make-rest-type
	  empty-rail-type?
	  rest-type?
	  head-type
	  tail-type

	  procedure-type
	  procedure-type-domain
	  procedure-type-codomain
	  restrictive?
	  procedure-type?
	  fixed-arity-procedure-type?
	  procedure-type-argument-types
	  procedure-type-arity
	  any-procedure-type
	  (proc :syntax)
	  (some-values :syntax)

	  boolean-type
	  char-type
	  null-type
	  unspecific-type

	  exact-integer-type
	  integer-type
	  rational-type
	  real-type
	  complex-type
	  number-type
	  exact-type
	  inexact-type

	  pair-type
	  string-type
	  symbol-type
	  vector-type

	  escape-type
	  structure-type

	  ;; Stuff moved back from syntactic - why was it moved there?
	  variable-type
	  variable-type?
	  variable-value-type
	  usual-variable-type
	  undeclared-type

	  compatible-types?))

(define-interface names-interface
  (export name?
	  name->symbol

	  make-name-generator
	  generate-name		;qualified->name in opt/inline.scm
	  generated?

	  generated-name
	  generated-token
	  generated-env
	  generated-parent-name
	  generated-uid

	  make-name-table

	  qualified?
	  qualified-parent-name
	  qualified-symbol
	  name->qualified

          desyntaxify
	  ))

(define-interface transforms-interface
  (export make-transform
	  maybe-apply-macro-transform
	  apply-inline-transform
	  transform?
	  transform-type

	  transform-env		; These are used to reify transforms.
	  transform-aux-names
	  transform-source
	  transform-id))

(define-interface bindings-interface
  (export binding?
	  make-binding
	  binding-place
	  set-binding-place!	;for package mutation, used in package.scm
	  binding-static
	  binding-type
	  binding-path

	  add-path
	  clobber-binding!
	  maybe-fix-place!
	  forget-integration
	  impose-type

	  same-denotation?
	  ))
	  
;----------------
; Syntactic interfaces

(define-interface compiler-envs-interface
  (export make-compiler-env	; re-exported by syntactic
	  lookup
	  bind1 bind bindrec

	  bind-source-file-name	; re-exported by syntactic
	  source-file-name

	  environment-macro-eval
	  environment-define!

	  extract-package-from-environment))	; temporary

(define-interface syntactic-interface
  (export expand
	  expand-form
	  scan-forms
	  expand-scanned-form
	  syntax?
	  make-compiler-env
	  bind-source-file-name))

(define-interface nodes-interface
  (export make-node

	  node?
	  node-operator
	  node-operator-id
	  node-form
	  node-ref
	  node-set!
	  node-predicate

	  make-similar-node
	  force-node
	  schemify

	  get-operator
	  make-operator-table
	  operator-name
	  operator-nargs
	  operator-table-ref
	  operator-define!
	  operator-lookup
	  operator-type
	  operator-uid
	  operator?
	  operators-table		;config.scm comp-package.scm
	  ))

;----------------
; Compiler interfaces

(define-interface segments-interface
  (export attach-label
	  byte-limit two-byte-limit
	  high-byte low-byte
	  empty-segment
	  instruction
	  instruction-using-label
	  instruction-with-literal
	  instruction-with-location
	  template
	  computed-goto-instruction
	  make-label
	  note-environment
	  note-source-code
	  segment->template  ;plan
	  segment-size
	  sequentially

	  ;; state
	  debug-flag-names	; keep and flush commands
	  debug-flag-accessor
	  debug-flag-modifier
	  debug-data-table
	  get-debug-data
	  with-fresh-compiler-state     ;for linker
	  keep-source-code?		;for bcomp
	  ))

(define-interface compiler-interface
  (export compile-forms
          make-startup-procedure	;link/link.scm
	  set-type-check?!		; for timings
	  define-compilator		;assem.scm
	  deliver-value			;assem.scm
	  ))

(define-interface primops-interface
  (export define-compiler-primitive
	  primop?
	  primop-name
	  primop-type
	  primop-closed
	  primop-compilator

	  get-primop
	  walk-primops			;comp-package.scm
	  ))

(define-interface debug-data-interface
  (export debug-data-env-maps
	  debug-data-name
	  debug-data-parent
	  debug-data-pc-in-parent
	  debug-data-source
	  debug-data-uid
	  debug-data?
	  set-debug-data-source!
	  set-debug-data-env-maps!
	  make-debug-data))

;----------------
; Module interfaces

; Interfaces.

(define-interface interfaces-interface
  (export make-simple-interface
	  make-compound-interface
	  make-modified-interface
	  note-reference-to-interface!
	  interface-ref
	  interface-member?
	  interface?
	  interface-clients
	  for-each-declaration
	  note-interface-name!))

; Packages.

(define-interface packages-interface
  (export make-package
	  make-simple-package		;start.scm
	  make-structure
	  make-modified-structure
	  package-define!
	  package-lookup
	  package?			;command.scm
	  structure-lookup		;env.scm
	  generic-lookup		;inline.scm
	  structure-interface		;config.scm
	  package->environment
	  link!
	  structure?
	  package-uid			;reifier
	  make-new-location		;ctop.scm
	  structure-package
	  note-structure-name!
	  (:package :type)
	  (:structure :type)))		;for (define-method ...)'s

(define-interface packages-internal-interface
  (export package-loaded?		;env/load-package.scm
	  set-package-loaded?!		;env/load-package.scm
	  package-name
	  flush-location-names
	  package-name-table		;debuginfo
	  location-info-table		;debuginfo, disclosers
	  package-unstable?		;env/pacman.scm
	  package-integrate?		;env/debug.scm
	  package-add-static!		;opt/analyze.scm
	  package-refine-type!		;opt/usage.scm
	  
	  ;; For scanner
	  for-each-export
	  package-accesses
	  package-clauses
	  package-file-name
	  package-opens
	  set-package-integrate?!
	  structure-name

	  ;; For package mutation
	  for-each-definition
	  set-package-opens!
	  set-structure-interface!
	  set-package-opens-thunk!
	  structure-clients
	  package-opens-thunk
	  package-opens-really
	  package-undefineds
	  package-undefined-but-assigneds
	  package-note-caching!
	  structure-interface-really
	  make-new-location
	  $get-location
	  set-package-get-location!
	  initialize-structure!
	  initialize-package!
	  location-for-reference

	  package-cached
	  package-definitions
	  package-clients))

(define-interface scan-package-interface
  (export collect-packages
	  package-source
	  package-optimizer-names
	  check-structure))

(define-interface optimizer-interface
  (export set-optimizer!
	  get-optimizer))

(define-interface undefined-interface
  (export noting-undefined-variables
	  $note-undefined		;env/command.scm
	  ))

;----------------
; Inlining

(define-interface inline-interface
  (export make-inline-transform
	  inline-transform		;bcomp/for-reify.scm
	  qualified->name))

(define-interface usages-interface
  (export find-usages
	  find-node-usages
	  usage-name-node
	  usage-reference-count
	  usage-operator-count
	  usage-assignment-count))

; End of compiler interfaces
;----------------

(define-interface environments-interface  ;cheesy
  (export *structure-ref
	  environment-define!
	  environment-ref
	  environment-set!
	  interaction-environment
	  make-reflective-tower
	  scheme-report-environment
	  null-environment
	  set-interaction-environment!
	  set-scheme-report-environment!
	  with-interaction-environment
	  set-reflective-tower-maker!))

(define-interface defpackage-interface
  (export ((def			        ;Formerly define-structure
	    define-interface		;Formerly define-signature
	    define-structures		;Formerly define-package
	    define-structure
	    define-module
	    define-syntax
	    define-reflective-tower-maker
	    export-reflective-tower-maker
	    compound-interface
	    modify subset with-prefix
	    export
	    structure structures let	; New
	    begin			; mostly for macros
	    define			; for compatibility?
	    a-package)			; cf. env/init-defpackage.scm
	   :syntax)
	  values
	  interface-of
	  set-verify-later!))

(define-interface types-interface
  (export ((:syntax :values :arguments :value) :type)
	  (procedure (proc (:type :type) :type))  ; (procedure T1 T2)
	  (proc :syntax)		; (proc (T1 ... Tn) T)
	  (some-values :syntax)		; (some-values T1 ... Tn)  ;JMG added it again
	  ((:boolean
	    :char
	    :number
	    :complex :real :rational :integer :exact-integer
	    :null
	    :unspecific
	    :pair
	    :string
	    :symbol
	    :vector
	    :procedure
	    :input-port :output-port
	    :error
	    :escape)
	   :type)
	  (:structure :type)
	  :type))		;Holy stratification, Batman!

(define-interface evaluation-interface
  (export eval load load-into eval-from-file
          load-port
	  ; eval-scanned-forms
	  ))

; VM architecture

(define-interface architecture-interface
  (export (enum :syntax)  ;So you don't have to remember to open ENUMERATED
	  (enumerand->name :syntax)
	  (name->enumerand :syntax)
	  bits-used-per-byte byte-limit
	  (exception :enumeration)
	  exception-count
	  (interrupt :enumeration)
	  interrupt-count
	  (memory-status-option :enumeration)
	  memory-status-option-count
	  (op :enumeration)
	  op-count
	  opcode-arg-specs
	  (stob :enumeration)
	  stob-count
	  stob-data
	  (channel-status-option :enumeration)
	  (time-option :enumeration)
	  time-option-count
	  (port-status-options :enumeration)
	  (current-port-marker :syntax)

	  maximum-stack-args
	  two-byte-nargs-protocol
	  two-byte-nargs+list-protocol
	  args+nargs-protocol
	  big-stack-protocol
	  nary-dispatch-protocol

	  default-stack-space
	  environment-stack-size
	  continuation-stack-size
	  available-stack-space))

(define-interface display-conditions-interface
  (export display-condition		;command.scm
	  &disclose-condition      	;env/disclosers.scm
	  limited-write))

; Bindings needed by the form composed by REIFY-STRUCTURES.

(define-interface for-reification-interface
  (export operator
	  simple-interface
	  package
	  transform
	  primop
	  ;; From usual-macros
	  usual-transform
	  ;; From inline
	  inline-transform
	  ;; From packages
	  package-define-static!
	  ;; From primops
	  make-structure
	  ))

;(define-interface command-interface ...)  -- moved to more-interfaces.scm
