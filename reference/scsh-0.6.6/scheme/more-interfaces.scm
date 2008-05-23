; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; Interfaces for packages that can get loaded after the initial.image
; starts up.

; Command processor

(define-interface command-processor-interface
  (export $write-length $write-depth
	  abort-to-command-level
	  add-sentinel!
	  command-continuation
	  command-threads
	  command-loop
          command-level-condition
	  command-processor
	  error-form ;foo
	  execute-command
	  exit-command-processor
	  evaluate-and-select
	  gobble-line
	  greet-user
	  environment-for-commands
	  focus-object
	  pop-command-level
	  read-command			;take
	  read-command-carefully	;inspect
	  read-form
	  run-sentinels
	  set-focus-object!
	  showing-focus-object		;inspect
	  start-command-processor
	  restart-command-processor
	  value->expression		;foo
	  write-carefully
	  write-line
	  y-or-n?
	  help
	  define-command-syntax
          define-user-command-syntax
	  user-command-environment
	  ;; set-command-structure!        ;startup
	  ;; command-structure             ;pacman
          set-user-command-environment! ;pacman
          read-command-error            ;inspect
	  &environment-id-string
	  ;&evaluate
	  ))

(define-interface command-levels-interface
  (export start-command-levels
	  command-levels
	  top-command-level
	  push-command-level
	  throw-to-command-level
	  restart-command-level
	  proceed-with-command-level
	  kill-paused-thread!

	  user-context
	  user-context-accessor
	  user-context-modifier

	  push-command-levels?

	  start-new-session
	  session-started?    ; for scsh
	  command-input
	  command-output
	  command-error-output
	  focus-values
	  set-focus-values!
	  batch-mode?
	  set-batch-mode?!
	  break-on-warnings?
	  set-break-on-warnings?!

	  reset-command-input?  ; condition predicate 

	  repl-data set-repl-data!

	  terminate-command-processor!
	  command-level
	  command-level-threads
	  command-level-paused-thread
	  command-level-repl-data
	  command-level?))

(define-interface basic-commands-interface
  (export exit
	  exit-when-done
          go
	  load
          help
          run
	  ?))
   
(define-interface build-commands-interface
  (export dump
          build))
       
(define-interface inspect-commands-interface
  (export inspect
	  debug
	  threads
	  where))
       
(define-interface disassemble-commands-interface
  (export dis))
   
(define-interface profile-commands-interface
  (export profile))
   
(define-interface package-commands-interface
  (export in
	  new-package
	  load-package
	  reload-package
	  structure
	  open
	  for-syntax
	  exec
	  user
	  user-package-is
	  config
	  config-package-is
	  undefine))

(define-interface debug-commands-interface
  (export translate
	  preview
	  proceed
	  push
	  pop
          reset
	  level
	  condition
	  batch
	  bench
	  break-on-warnings
	  form-preferred
	  levels
	  flush
	  keep
          collect
	  trace
	  untrace
	  time
          from-file
	  forget
	  bound?
	  expand))

(define-interface usual-commands-interface
  (compound-interface
   basic-commands-interface
   build-commands-interface
   package-commands-interface
   debug-commands-interface
   inspect-commands-interface
   disassemble-commands-interface
   ;profile-commands-interface
   ))

(define-interface package-commands-internal-interface
  (export config-package
	  new-command-processor
	  get-structure
	  user-environment   ;JMG 3 for scsh
	  get-reflective-tower
	  in-package
	  ;get-package
	  ;set-package-evaluator!
	  ))

(define-interface debuginfo-interface
  (export read-debug-info
	  write-debug-info))

(define-interface disclosers-interface
  (export make-print-name
	  template-file-name
	  value->expression
	  error-form
	  location-info
	  location-name
	  template-debug-data
	  template-id
	  template-name
	  template-names
	  debug-data-names))

(define-interface package-mutation-interface
  (export package-system-sentinel	;env/command.scm
	  package-open!			;env/debug.scm
	  package-undefine!
	  ))

(define-interface packages-cruft-interface
  (export assume-denotation
	  ;; new-location-uid    ;?
	  interface-ref
	  structure-interface
	  verify-package		;for debugging the package system
	  ))


; --------------------
; Linker

(define-interface linker-interface
  (export link-simple-system
	  link-reified-system
	  link-semireified-system
	  (struct-list :syntax)
	  compile-structures))

(define-interface expander-interface
  (export expand-form
	  expand-stuff
	  expand
	  usage-reference-count
	  usage-operator-count
	  usage-assignment-count
	  free-top-level-variables))

; --------------------
; Extended numbers: bignums, ratnums, etc.

(define-interface extended-numbers-interface
  (export (define-extended-number-type :syntax)
	  (define-opcode-extension :syntax)
	  :exact :inexact
	  string-position
	  &+
	  &-
	  &*
	  &/
	  &=
	  &<
	  &quotient
	  &remainder
	  &integer?
	  &rational?
	  &real?
	  &complex?
	  &number?
	  &exact?
	  &exact->inexact
	  &inexact->exact
	  &real-part
	  &imag-part
	  &floor
	  &numerator
	  &denominator
	  &exp &log
	  &sin &cos &tan &asin &acos &atan
	  &sqrt
	  &make-rectangular
	  &number->string
	  &really-string->number))

(define-interface bignums-interface  ;Things used by bigbit
  (export integer->bignum bignum-magnitude bignum-sign
	  zero-magnitude?
	  integer->magnitude
	  adjoin-digit
	  low-digit high-digits
	  make-integer
	  zero-magnitude
	  radix log-radix
	  integer-negate integer- integer=))

(define-interface time-interface
  (export real-time
	  run-time))

; Experimental DEFINE-RECORD-TYPE that is now officially a failure.

(define-interface defrecord-interface  ;RK's
  (export (define-record-type :syntax)
	  define-record-discloser))

; --------------------
; Big Scheme

(define-interface dynamic-externals-interface
  (export dynamic-load

          get-external
	  lookup-external
	  lookup-all-externals

	  external?
	  external-name
	  external-value
	  external-lookup

	  call-external))

(define-interface dump/restore-interface
  (export dump
	  restore
	  note-location!
	  $dump-index
	  $restore-index))

(define-interface extended-ports-interface
  (export char-source->input-port
	  char-sink->output-port
	  make-tracking-input-port make-tracking-output-port
	  make-string-input-port
	  make-string-output-port
	  string-output-port-output
	  call-with-string-output-port		; denigrated
	  write-one-line
	  current-row current-column fresh-line))

(define-interface arrays-interface
  (export make-array		; <initial-value> <bound1> ...
	  array?
	  array-shape		; <array>
	  array-ref		; <array> <index1> ...
	  array-set!		; <array> <value> <index1> ...
	  make-shared-array	; <array> <linear-map> <bound1> ...
	  copy-array		; <array>
	  array->vector		; <array>
	  array))		; <bounds> . <elements>

(define-interface enum-sets-interface
  (export (define-enum-set-type :syntax)
	  enum-set->list
	  enum-set-member?
	  enum-set=?
	  enum-set-union
	  enum-set-intersection
	  enum-set-negation))

(define-interface enum-sets-internal-interface
  (export enum-set-has-type?
	  enum-set?
	  enum-set-type
	  enum-set->integer
	  integer->enum-set))

(define-interface thread-fluids-interface
  (export make-thread-fluid
	  thread-fluid
	  let-thread-fluid
	  let-thread-fluids
	  set-thread-fluid!
	  make-preserved-thread-fluid
	  preserve-thread-fluids
	  fork-thread
          spoon))

(define-interface search-trees-interface
  (export make-search-tree
	  search-tree?
          search-tree-ref
          search-tree-set!
          search-tree-modify!
          search-tree-max pop-search-tree-max!
          search-tree-min pop-search-tree-min!
          walk-search-tree))

; This is getting to be a hodge-podge.

(define-interface big-util-interface       
  (export concatenate-symbol
	  error breakpoint
	  atom? null-list? neq? n=
	  identity no-op
	  memq? first any? any every?
	  filter filter! filter-map partition-list partition-list!
	  remove-duplicates delq delq! delete
	  reverse!
	  copy-string
	  string->immutable-string
	  ))

(define-interface big-scheme-interface
  (compound-interface
      (interface-of ascii)
      (interface-of bitwise)
      (interface-of tables)
      (interface-of enumerated)
      ;defrecord-interface
      extended-ports-interface
      big-util-interface
      (export (destructure :syntax)
	      (receive :syntax)
	      format
	      p pretty-print
	      sort-list sort-list!)))

; --------------------
; Miscellaneous

; Copied from interfaces.scm.
(define-interface define-record-types-interface
  (export (define-record-type :syntax)
	  define-record-discloser))

(define-interface placeholder-interface
  (export make-placeholder
	  placeholder?
	  placeholder-value
	  placeholder-set!))

(define-interface sicp-interface
  (export and or (sequence :syntax)
	  mapcar mapc 1+ -1+ t nil atom? print princ prin1 error
	  (cons-stream :syntax) head tail the-empty-stream empty-stream?
	  explode implode get put))

; Olin's encyclopedic SRFIs.

(define-interface srfi-1-interface
  (export map for-each member assoc	; redefined from R5RS
	  xcons make-list list-tabulate cons* list-copy 
	  proper-list? circular-list? dotted-list? not-pair? null-list? list=
	  circular-list length+
	  iota
	  first second third fourth fifth sixth seventh eighth ninth tenth
	  car+cdr
	  take       drop       
	  take-right drop-right 
	  take!      drop-right!
	  split-at   split-at!
	  last last-pair
	  zip unzip1 unzip2 unzip3 unzip4 unzip5
	  count
	  append! append-reverse append-reverse! concatenate concatenate! 
	  unfold       fold       pair-fold       reduce
	  unfold-right fold-right pair-fold-right reduce-right
	  append-map append-map! map! pair-for-each filter-map map-in-order
	  filter  partition  remove
	  filter! partition! remove! 
	  find find-tail any every list-index
	  take-while drop-while take-while!
	  span break span! break!
	  delete delete!
	  alist-cons alist-copy
	  delete-duplicates delete-duplicates!
	  alist-delete alist-delete!
	  reverse! 
	  lset<= lset= lset-adjoin  
	  lset-union  lset-intersection  lset-difference  lset-xor
	  lset-diff+intersection
	  lset-union! lset-intersection! lset-difference! lset-xor!
	  lset-diff+intersection!))

(define-interface srfi-13-interface
  (export string-map string-map!
	  string-fold       string-unfold
	  string-fold-right string-unfold-right 
	  string-tabulate string-for-each string-for-each-index
	  string-every string-any
	  string-hash string-hash-ci
	  string-compare string-compare-ci
	  string=    string<    string>    string<=    string>=    string<>
	  string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
	  string-downcase  string-upcase  string-titlecase  
	  string-downcase! string-upcase! string-titlecase! 
	  string-take string-take-right
	  string-drop string-drop-right
	  string-pad string-pad-right
	  string-trim string-trim-right string-trim-both
	  string-filter string-delete
	  string-index string-index-right 
	  string-skip  string-skip-right
	  string-count
	  string-prefix-length string-prefix-length-ci
	  string-suffix-length string-suffix-length-ci
	  string-prefix? string-prefix-ci?
	  string-suffix? string-suffix-ci?
	  string-contains string-contains-ci
	  string-copy! substring/shared
	  string-reverse string-reverse! reverse-list->string
	  string-concatenate
	  string-concatenate/shared
	  string-concatenate-reverse
	  string-concatenate-reverse/shared
	  string-append/shared
	  xsubstring string-xcopy!
	  string-null?
	  string-join
	  string-tokenize
	  string-replace

	  string->list string-copy string-fill! 
	  string? make-string string-length string-ref string-set! 
	  string string-append list->string))

(define-interface srfi-14-interface
  (export char-set? char-set=
	  char-set<= char-set-hash char-set-cursor char-set-ref
	  char-set-cursor-next end-of-char-set? char-set-fold char-set-unfold
	  char-set-unfold!  char-set-for-each char-set-map char-set-copy
	  char-set

	  list->char-set  string->char-set 
	  list->char-set! string->char-set! 

	  char-set-filter  ucs-range->char-set

	  ; the SRFI defines ->CHAR-SET, but that isn't a legal identifier
	  x->char-set
	  
	  char-set-filter! ucs-range->char-set!

	  char-set->list char-set->string

	  char-set-size char-set-count char-set-contains?
	  char-set-every char-set-any

	  char-set-adjoin  char-set-delete 
	  char-set-adjoin! char-set-delete!
 

	  char-set-complement  char-set-union  char-set-intersection  
	  char-set-complement! char-set-union! char-set-intersection! 

	  char-set-difference  char-set-xor  char-set-diff+intersection
	  char-set-difference! char-set-xor! char-set-diff+intersection!

	  char-set:lower-case	char-set:upper-case	char-set:title-case
	  char-set:letter	char-set:digit		char-set:letter+digit
	  char-set:graphic	char-set:printing	char-set:whitespace
	  char-set:iso-control	char-set:punctuation	char-set:symbol
	  char-set:hex-digit	char-set:blank		char-set:ascii
	  char-set:empty	char-set:full

	  ))

(define-interface srfi-19-interface
  (export;; Constants
   time-duration
   time-monotonic
   time-process
   time-tai
   time-thread
   time-utc
   ;; Current time and clock resolution
   current-date
   current-julian-day
   current-modified-julian-day
   current-time
   time-resolution
   ;; Time object and accessors
   make-time
   time?
   time-type
   time-nanosecond
   time-second
   set-time-type!
   set-time-nanosecond!
   set-time-second!
   copy-time
   ;; Time comparison procedures
   time<=?
   time<?
   time=?
   time>=?
   time>?
   ;; Time arithmetic procedures
   time-difference
   time-difference!
   add-duration
   add-duration!
   subtract-duration
   subtract-duration!
   ;; Date object and accessors
   make-date
   date?
   date-nanosecond
   date-second
   date-minute
   date-hour
   date-day
   date-month
   date-year
   date-zone-offset
   date-year-day
   date-week-day
   date-week-number
   ;; Time/Date/Julian Day/Modified Julian Day converters
   date->julian-day
   date->modified-julian-day
   date->time-monotonic
   date->time-tai
   date->time-utc
   julian-day->date
   julian-day->time-monotonic
   julian-day->time-tai
   julian-day->time-utc
   modified-julian-day->date
   modified-julian-day->time-monotonic
   modified-julian-day->time-tai
   modified-julian-day->time-utc
   time-monotonic->date
   time-monotonic->time-tai
   time-monotonic->time-tai!
   time-monotonic->time-utc
   time-monotonic->time-utc!
   time-tai->date
   time-tai->julian-day
   time-tai->modified-julian-day
   time-tai->time-monotonic
   time-tai->time-monotonic!
   time-tai->time-utc
   time-tai->time-utc!
   time-utc->date
   time-utc->julian-day
   time-utc->modified-julian-day
   time-utc->time-monotonic
   time-utc->time-monotonic!
   time-utc->time-tai
   time-utc->time-tai!
   ;; Date to string/string to date converters.
   date->string
   string->date))

(define-interface srfi-27-interface
  (export random-integer
	  random-real
	  default-random-source
	  make-random-source
	  random-source?
	  random-source-state-ref
	  random-source-state-set!
	  random-source-randomize!
	  random-source-pseudo-randomize!
	  random-source-make-integers
	  random-source-make-reals))

(define-interface srfi-31-interface
  (export (rec :syntax)))

(define-interface srfi-37-interface
  (export
   option
   option?
   option-names
   option-required-arg?
   option-optional-arg?
   option-processor
   args-fold))

(define-interface srfi-42-interface
  (export ((do-ec
	    list-ec append-ec
	    string-ec string-append-ec
	    vector-ec vector-of-length-ec
	    sum-ec product-ec
	    min-ec max-ec
	    any?-ec every?-ec
	    first-ec last-ec
	    fold-ec fold3-ec) :syntax)
	  ((:
	    :list :string :vector
	    :integers
	    :range :real-range :char-range
	    :port
	    :dispatched) :syntax)
	  ((:do :let :parallel :while :until) :syntax)
	  :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
	  (:generator-proc :syntax)))