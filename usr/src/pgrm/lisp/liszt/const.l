
;;; ----	c o n s t		header file for inclusion

;
;$Header: const.l,v 1.4 87/12/15 16:58:19 sklower Exp $
;

(putprop 'const t 'loaded)	; flag that this file has been loaded

;--- parameters: these must be evaluated at compile time so readmacros will
; work
(eval-when (compile eval)
   (setq np-sym "_np"
	 lbot-sym "_lbot"
	 bnp-sym "_bnp"
	 ch-newline (ascii #\lf))
   #+(or for-vax for-tahoe)
   (setq np-reg 'r6
	 lbot-reg 'r7
	 bind-reg 'r8
	 np-plus '(+ r6)
	 np-minus '(- r6)
	 bnp-reg 'r5
	 fixnum-reg 'r5
	 olbot-reg 'r10
	 nil-reg "*** this should never be used ***"
	 comment-char "#"
	 Cstack "-(sp)"
	 unCstack "(sp)+"
	 Nilatom "$0")
   #+for-68k
   (setq bnp-reg 'a1
	 np-reg 'a2
	 lbot-reg 'd3		; only for hacked version of 68k lisp
	 bind-reg 'a3
	 olbot-reg 'a4
	 fixnum-reg 'd2
	 nil-reg 'd7
	 np-plus '(+ a2)
	 np-minus '(- a2)
	 Nilatom "#_nilatom"
	 comment-char "|"
	 Cstack "sp@-"
	 unCstack "sp@+"))

;--- evaluation frame parameters.  These must correspond to what is 
; in h/frame.h in the C system
;
(eval-when (compile load eval)
  (setq C_GO 1   	C_RET 2   	C_THROW 3   	C_RESET 4)
  (setq F_PROG 1 	F_CATCH 2 	F_RESET 3))

; offsets in frame
(setq OF_olderrp 12)	; 12 bytes from base of error frame
