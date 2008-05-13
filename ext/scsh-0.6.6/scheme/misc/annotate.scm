; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.



; Derived from
;  (lambda (x y) (cons (car x) (cdr x)))

(define annotate-procedure
  (lap annotate-procedure ()
       0 (protocol 2)
       2 (make-env 2)
       4 (local0 2)
       6 (stored-object-ref closure 0)
       9 (push)
       10 (local0 2)
       12 (stored-object-ref closure 1)
          (push)
	  (local0 1)
       15 (make-stored-object 3 closure)
       18 (return)))

; Derived from
;  (lambda (x) (if (< 2 (vector-length x)) (vector-ref x 2) #f))

(define procedure-annotation
  (lap procedure-anotation ()
       0 (protocol 1)
       2 (make-env 1)
       4 (literal '2)
       6 (push)
       7 (local0 1)
       9 (stored-object-length closure)
       11 (<)
       12 (jump-if-false (=> 23))
       15 (local0 1)
       20 (stored-object-ref closure 2)
       22 (return)
       23 (false)
       24 (return)))
