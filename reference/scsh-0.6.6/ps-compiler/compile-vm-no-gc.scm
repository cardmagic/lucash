; Copyright (c) 1993-1999 by Richard Kelsey.  See file COPYING.

(config '(load "../scheme/vm/macro-package-defs.scm"))
(load-package 'vm-architecture)
(in 'forms '(run (set! *duplicate-lambda-size* 30)))
(in 'simplify-let '(run (set! *duplicate-lambda-size* 15)))
(in 'prescheme-compiler
    '(run (prescheme-compiler
	   '(vm external-gc-roots)
	   '("../scheme/vm/interfaces.scm"
	     "../scheme/vm/ps-package-defs.scm"
	     "../scheme/vm/package-defs.scm"
	     "../scheme/vm/no-gc-package-defs.scm")
	   's48-init
	   "../scheme/vm/scheme48vm.c"
	   '(header "#include \"scheme48vm-prelude.h\"")
	   '(copy (interpreter push-continuation-on-stack))
	   '(no-copy (interpreter interpret
				  application-exception
				  handle-interrupt
				  uuo)
		     ;(vm restart)
		     (interpreter-gc collect-saving-temp
				     collect-saving-temps)))))
;	   '(shadow ((interpreter restart)
;		     (interpreter *val* *code-pointer*)
;		     (stack *stack* *env*))))))
