; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.

(define-record-type thread :thread
  (make-thread dynamic-env dynamic-point
	       cell-values own-cell-values?)
  (dynamic-env   thread-dynamic-env)
  (dynamic-point thread-dynamic-point)
  (cell-values    thread-cell-values set-thread-cell-values!)
  (own-cell-values? thread-own-cell-values? set-thread-own-values?!))

(define (empty-cell-values) '())

(define (get-cell-values)
  (record-ref (current-thread) 3))

(define (set-cell-values! values)
  (record-set! (current-thread) 3 values))

(define (get-own-cell-values?)
  (record-ref (current-thread) 4))

(define (set-own-cell-values? own-values?)
  (record-set! (current-thread) 4 own-values?))

(define-record-type thread-cell :thread-cell
  (make-thread-cell top)
  (top thread-cell-top-level-value set-thread-cell-top-level-value!))

(define (thread-cell-ref thread-cell)
  (cond
   ((assq thread-cell (get-cell-values)) => cdr)
   (else (thread-cell-top-level-value thread-cell))))

(define (thread-cell-set! thread-cell val)
  (cond
   ;; This might benefit from reordering: if we don't have a binding
   ;; here, it's safe to set cell-values regardless of the setting of
   ;; OWN-CELL-VALUES?.  On the other hand, this may mean we copy too
   ;; much when push comes to shove; probably best to store the
   ;; original CELL-VALUES instead of OWN-CELL-VALUES?.
   ((not (get-own-cell-values?))
    (let loop ((values (get-cell-values))
	       (rev-new-values '())
	       (found? #f))
      (cond
       ((null? values)
	(set-cell-values! (if found?
			      (reverse rev-new-values)
			      (cons (cons thread-cell val)
				    (reverse rev-new-values))))
	(set-own-cell-values? #t))
       ((eq? thread-cell (caar values))
	(loop (cdr values)
	      (cons (cons (caar values) val)
		    rev-new-values)
	      #t))
       (else
	(loop (cdr values)
	      (cons (cons (caar values) (cdar values))
		    rev-new-values)
	      found?)))))
   ((assq thread-cell (get-cell-values))
    => (lambda (pair)
	 (set-cdr! pair val)))
   (else
    (set-cell-values! (cons (cons thread-cell val)
			    (get-cell-values))))))

(define (initialize-dynamic-state!)
  (set-current-thread! (make-thread (empty-dynamic-env) #f
				    (empty-cell-values) #t)))


(initialize-dynamic-state!)
