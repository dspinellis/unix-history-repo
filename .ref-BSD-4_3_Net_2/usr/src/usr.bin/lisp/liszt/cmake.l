; file which loads in all the object files and dumps them

; $Header: cmake.l,v 1.7 87/12/15 16:57:01 sklower Exp $

;				-[Sat Aug 13 18:03:38 1983 by layer]-

;--- genl :: generate liszt
; args are unevalated.  first arg is the name of the liszt to build
; other args [optional]:  slow		- build interpreted.
;
(defun genl fexpr (args)
   (let ((dumpname (car args))
	 (slowp (memq 'slow (cdr args))))
      (load 'fcninfo)	; in /usr/lib/lisp (not normally in lisp)
      (if slowp then (load '../cmacros.l))
      (mapc '(lambda (name)
		(if slowp
		   then ; lisp source is in ..
			(load (concat "../" name ".l"))
		   else ; objects are in .
			(load name)))
	    '(decl array vector datab expr io funa funb func tlev
		   instr fixnum util lversion))
      (allocate 'list 400)
      (allocate 'fixnum 10)
      (allocate 'symbol 50)
      (sstatus translink on)
      (if slowp then (setq displace-macros t))
      (sstatus nofeature for-vax) ; remove memory of what it was compiled for
      (sstatus nofeature for-tahoe)
      (sstatus nofeature for-68k)
      ;indicate type of compiler (np and lbot in global registers)
      (setq $global-reg$ (not (status feature no-global-reg)))
      (putprop 'chead nil 'version)  ; so the compiler can compile itself
      (setq ER%tpl 'break-err-handler) ; in case we are using another tpl
      ; this is a temporary measure
      (apply 'dumplisp (list dumpname))))
