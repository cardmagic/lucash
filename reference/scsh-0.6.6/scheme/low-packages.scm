; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; Things provided by the byte compiler / VM, together with a few
; things with rather sensitive definitions (low.scm).

(define-structures ((scheme-level-0 scheme-level-0-interface)
		    (primitives primitives-interface)
		    (bitwise bitwise-interface)
		    (closures closures-interface)
		    (byte-vectors byte-vectors-interface)
		    (code-vectors code-vectors-interface)
		    (write-images (export write-image))	;for linker
		    (source-file-names (export (%file-name% :syntax)))
		    (loopholes (export (loophole :syntax)))
		    (low-level low-level-interface)
		    (escapes escapes-interface)
		    (vm-exposure vm-exposure-interface)
		    (ascii ascii-interface)
		    (locations locations-interface)
		    (cells cells-interface)
		    (low-channels low-channels-interface)
		    (ports ports-interface)
		    (shared-bindings shared-bindings-interface)
		    (signals signals-interface)
		    (debug-messages (export debug-message))
		    (silly (export reverse-list->string))
		    (code-quote (export (code-quote :syntax)))
		    (structure-refs (export (structure-ref :syntax))))
  (define-all-operators)		; Primitive Scheme, in the LSC paper
  (usual-transforms and cond do let let* or)
  (files (rts low)
	 (rts signal))
  (optimize auto-integrate))

