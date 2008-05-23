; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.



; Closures

(define closure-rtd      (make-record-type 'closure '(template env)))
(define closure?         (record-predicate closure-rtd))
(define make-closure     (record-constructor closure-rtd '(template env)))
(define closure-template (record-accessor closure-rtd 'template))
(define closure-env	 (record-accessor closure-rtd 'env))
