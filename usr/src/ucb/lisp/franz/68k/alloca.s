| /* Copyright (c) 1982, Regents, University of California */
	.text
	.globl	_alloca
_alloca:
	movl	sp@,d0
	movl	sp@(4),d1
	subl	#1,d1
	orl	#3,d1
	addl	#1,d1
	subl	d1,sp
	tstb	sp@(-132)
	movl	d0,sp@
	movl	sp,d0
	addl	#8,d0
	rts
