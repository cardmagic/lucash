(define-interface test-base-interface
  (export add-test!
	  add-test-multiple!
	  test-all
	  test-group
	  test-single
	  test-single/args
	  test-summary))

(define-structure test-base test-base-interface
  (open scheme-with-scsh
        handle
	list-lib
	define-record-types)
  (files test-base))

(define-structure file-system-test (export)
  (open	scheme-with-scsh
	test-base)
  (files file-system-tests))

(define-structure process-state-test (export)
  (open	scheme-with-scsh
	test-base)
  (files process-state-tests))

(define-structure env-test 
  (export   
   setenv-test
   getenv-test
   env->alist-test
   alist->env-test
   alist-delete-test
   alist-update-test
   alist-compress-test
   with-env*-test
   with-total-env*-test
   home-directory-test
   exec-path-list-test
   add-before-test
   add-after-test)
  (open scheme-with-scsh
 	thread-fluids
	list-lib
	string-lib)
  (files env-test-code))
  
(define-structure add-env-test
  (export)
  (open scheme-with-scsh
	test-base
        env-test)
  (files env-test-add))

(define-structure system-parameter-tests (export)
  (open scheme-with-scsh
	test-base)
  (begin
    (add-test! 'uname 'system-parameter
	       (lambda ()
		 (let ((uname-rec (uname)))
		   (> (string-length (uname:node-name uname-rec)) 0))))

    (add-test! 'system-name 'system-parameter
	       (lambda ()
		 (> (string-length (system-name)) 0)))))

(define-structure strings-and-chars-test (export)
  (open scheme-with-scsh
	test-base)
  (files strings-and-chars-test))

(define-structure test-all
  (export test-all)
  (open scheme
	test-base
	add-env-test
	process-state-test
	system-parameter-tests
	file-system-test
	strings-and-chars-test))

    
	
