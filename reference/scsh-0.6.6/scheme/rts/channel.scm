; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Provides input and output channels.

(define (input-channel? thing)
  (and (channel? thing)
       (or (= (channel-status thing)
	      (enum channel-status-option input))
	   (= (channel-status thing)
	      (enum channel-status-option special-input)))))

(define (output-channel? thing)
  (and (channel? thing)
       (or (= (channel-status thing)
	      (enum channel-status-option output))
	   (= (channel-status thing)
	      (enum channel-status-option special-output)))))

;;; Scsh changes umask and cwd lazily
;;; placeholder until scsh sets it
(define (with-fs-context-aligned* thunk)
  (thunk))

(define (set-with-fs-context-aligned*! wpca*)
  (set! with-fs-context-aligned* wpca*))


(define (open-input-channel filename)
  (let ((channel (with-fs-context-aligned*
		  (lambda ()
		    (open-channel filename (enum channel-status-option input))))))
    (if (channel? channel)
	channel
	(error "cannot open input file" filename))))

(define (open-output-channel filename)
  (let ((channel (with-fs-context-aligned*
		  (lambda ()
		    (open-channel filename (enum channel-status-option output))))))
    (if (channel? channel)
	channel
	(error "cannot open output file" filename))))

(define (close-input-channel channel)
  (if (input-channel? channel)
      (close-channel channel)
      (call-error close-input-channel channel)))

(define (close-output-channel channel)
  (if (output-channel? channel)
      (close-channel channel)
      (call-error close-output-channel channel)))
