; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.
; Code for handling interrupts.

; New interrupt handler vector in *val*

(define-opcode set-interrupt-handlers!
  (cond ((or (not (vm-vector? *val*))
	     (< (vm-vector-length *val*) interrupt-count))
	 (raise-exception wrong-type-argument 0 *val*))
	(else
	 (let ((temp *interrupt-handlers*))
	   (set! *interrupt-handlers* *val*)
	   (set! *val* temp)
	   (goto continue 0)))))

; New interrupt mask as fixnum in *val*

(define-opcode set-enabled-interrupts!
  (let ((old *enabled-interrupts*))
    (set-enabled-interrupts! (extract-fixnum *val*))
    (set! *val* (enter-fixnum old))
    (goto continue 0)))

; Save the current interpreter state and call an interrupt handler.

(define (handle-interrupt stack-arg-count)
  (push *val*)
  (push *template*)
  (push (current-pc))
  (push (enter-fixnum *enabled-interrupts*))
  (set-template! *interrupt-template* (enter-fixnum 0))
  (push-continuation! *code-pointer* (+ stack-arg-count 4))
  (let* ((pending-interrupt (get-highest-priority-interrupt!))
	 (arg-count (push-interrupt-args pending-interrupt)))
    (if (not (vm-vector? *interrupt-handlers*))
	(error "interrupt handler is not a vector"))
    (set-enabled-interrupts! 0)
    (set! *val* (vm-vector-ref *interrupt-handlers* pending-interrupt))
    (if (not (closure? *val*))
	(error "interrupt handler is not a closure" pending-interrupt))
    (goto call-interrupt-handler arg-count pending-interrupt)))

; Push the correct arguments for each type of interrupt.
;
; For alarm interrupts the interrupted template is passed to the handler
;  for use by code profilers.
; For gc interrupts we push the list of things to be finalized.
; For i/o-{read,write}-completion we push the channel and its status.

(define (push-interrupt-args pending-interrupt)
  (cond ((eq? pending-interrupt (enum interrupt alarm))
	 (push *interrupted-template*)
	 (set! *interrupted-template* false)
	 (push (enter-fixnum *enabled-interrupts*))
	 2)
	((eq? pending-interrupt (enum interrupt post-gc))
	 (push *finalize-these*)
	 (set! *finalize-these* null)
	 (push (enter-fixnum *enabled-interrupts*))
	 2)
	((eq? pending-interrupt (enum interrupt i/o-read-completion))
	 (let ((channel (dequeue-input-channel!)))
	   (if (not (input-channel-queue-empty?))
	       (note-interrupt! pending-interrupt))
	   (push channel)
	   (push (channel-os-status channel))
	   (push (enter-fixnum *enabled-interrupts*))
	   3))
	((eq? pending-interrupt (enum interrupt i/o-write-completion))
	 (let ((channel (dequeue-output-channel!)))
	   (if (not (output-channel-queue-empty?))
	       (note-interrupt! pending-interrupt))
	   (push channel)
	   (push (channel-os-status channel))
	   (push (enter-fixnum *enabled-interrupts*))
	   3))
	((eq? pending-interrupt (enum interrupt os-signal))
	 (push (vm-car *os-signal-list*))
	 (set! *os-signal-list* (vm-cdr *os-signal-list*))
	 (if (not (vm-eq? *os-signal-list* null))
	     (note-interrupt! (enum interrupt os-signal)))
	 (push (enter-fixnum *enabled-interrupts*))
	 2)
	(else
	 (push (enter-fixnum *enabled-interrupts*))
	 1)))

; Called from outside when an os-signal event is returned.

(define (s48-set-os-signals signal-list)
  (set! *os-signal-list* (vm-append! *os-signal-list* signal-list)))

(define *os-signal-list* null)

; Return from a call to an interrupt handler.

(define-opcode return-from-interrupt
  (set-enabled-interrupts! (extract-fixnum (pop)))
  (let ((pc (pop)))
    (set-template! (pop) pc))
  (set! *val* (pop))
  (goto interpret *code-pointer*))

; Do nothing much until something happens.  To avoid race conditions this
; opcode is called with all interrupts disabled, so it has to return if
; any interrupt occurs, even a disabled one.

(define-primitive wait (fixnum-> boolean->)
  (lambda (max-wait minutes?)
    (if (not (any-pending-interrupts?))
	(wait-for-event max-wait minutes?))
    (goto return-unspecific 0)))

; The players:
;   pending-interrupts-X      A bit mask of pending interrupts
;   *enabled-interrupts*      A bit mask of enabled interrupts
;   s48-*pending-interrupt?*  True if either an event or interrupt is pending
;   s48-*pending-events?*     True if an event is pending
;
; When an asynchronous event occurs the OS sets S48-*PENDING-EVENTS?* and
; S48-*PENDING-INTERRUPT?* to true.
;
; When S48-*PENDING-EVENTS?* is true the VM calls (CURRENT-EVENTS) to get the
; pending events.
;
; The goals of all this mucking about are:
;   - no race conditions
;   - the VM operates synchronously; only the OS is asynchronous
;   - polling only requires testing S48-*PENDING-INTERRUPT?*

(define s48-*pending-events?* #f)

; Called asynchronously by the OS

(define (s48-note-event)
  (set! s48-*pending-events?* #t)       ; order required by non-atomicity
  (set! s48-*pending-interrupt?* #t))

; The polling procedure.  If *PENDING-INTERRUPT* then either there is a
; pending OS event or there is really a pending interrupt.  CHECK-EVENTS
; ends with a call to PENDING-INTERRUPT? (we don't call it here because
; we want this procedure to be small so that it will compiled in-line).

(define (pending-interrupt?)
  (cond ((not s48-*pending-interrupt?*)
	 #f)
	(s48-*pending-events?*
	 (set! s48-*pending-events?* #f)
	 (check-events))  ; ends with call to PENDING-INTERRUPT?
	(else
	 #t)))

; Same, except we include disabled interrupts.

(define (any-pending-interrupts?)
  (or (pending-interrupt?)
      (not (pending-interrupts-empty?))))

; Add INTERRUPT to the set of pending interrupts, then check to see if it
; is currently pending.

(define (note-interrupt! interrupt)
  (pending-interrupts-add! (interrupt-bit interrupt))
  (check-for-enabled-interrupt!))

; Remove INTERRUPT from the set of pending interrupts, then recheck for pending
; interrupts; INTERRUPT may have been the only one.

(define (clear-interrupt! interrupt)
  (pending-interrupts-remove! (interrupt-bit interrupt))
  (check-for-enabled-interrupt!))

; Install a new set of enabled interrupts.  As usual we have to recheck for
; enabled interrupts.

(define (set-enabled-interrupts! enabled)
  (set! *enabled-interrupts* enabled)
  (check-for-enabled-interrupt!))

; Disable all interrupts.

(define (disable-interrupts!)
  (set! s48-*pending-interrupt?* #f)
  (set! *enabled-interrupts* 0))

; Enable all interrupts.

(define (enable-interrupts!)
  (set-enabled-interrupts! -1))

; Whenever *PENDING-INTERRUPTS* or *ENABLED-INTERRUPTS* changes we have to
; set S48-*PENDING-INTERRUPT?* to the correct value.

(define (check-for-enabled-interrupt!)
  (if (= 0 (bitwise-and (pending-interrupts-mask) *enabled-interrupts*))
      (begin
	(set! s48-*pending-interrupt?* #f)  ; Done first to avoid a race condition.
	(if s48-*pending-events?*
	    (set! s48-*pending-interrupt?* #t)))
      (set! s48-*pending-interrupt?* #t)))

; We don't need to mess with S48-*PENDING-INTERRUPT?* because all interrupts
; are about to be disabled.

(define (get-highest-priority-interrupt!)
  (let ((n (bitwise-and (pending-interrupts-mask) *enabled-interrupts*)))
    (let loop ((i 0) (m 1))
      (cond ((= 0 (bitwise-and n m))
	     (loop (+ i 1) (* m 2)))
	    (else
	     (pending-interrupts-remove! m)
	     i)))))

; Process any pending OS events.  PROCESS-EVENT returns a mask of any interrupts
; that have just occured.

(define (check-events)
  (receive (type channel status)
      (get-next-event)
    (pending-interrupts-add! (process-event type channel status))
    (if (eq? type (enum events no-event))
	(begin
	  (check-for-enabled-interrupt!)
	  (pending-interrupt?))
	(check-events))))

; Do whatever processing the event requires.

(define (process-event event channel status)
  (cond ((eq? event (enum events alarm-event))
	 ;; Save the interrupted template for use by profilers.
	 (if (false? *interrupted-template*)
	     (set! *interrupted-template* *template*))
	 (interrupt-bit (enum interrupt alarm)))
	((eq? event (enum events keyboard-interrupt-event))
	 (interrupt-bit (enum interrupt keyboard)))
	((eq? event (enum events io-read-completion-event))
	 (enqueue-input-channel! channel status)
	 (interrupt-bit (enum interrupt i/o-read-completion)))
	((eq? event (enum events io-write-completion-event))
	 (enqueue-output-channel! channel status)
	 (interrupt-bit (enum interrupt i/o-write-completion)))
	((eq? event (enum events os-signal-event))
	 (interrupt-bit (enum interrupt os-signal)))
	((eq? event (enum events no-event))
	 0)
	((eq? event (enum events error-event))
	 (error-message "OS error while getting event")
	 (error-message (error-string status))
	 0)
	(else
	 (error-message "unknown type of event")
	 0)))



