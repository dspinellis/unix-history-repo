| /* Copyright (c) 1982, Regents, University of California */
	.text
	.globl	_stack
_stack:
	movl	sp@,a0
	jmp	a0@
	.globl	_unstack
_unstack:
	movl	sp@+,a0
	movl	sp@+,d0
	jmp	a0@
	.globl	_sp
_sp:
	movl	sp@+,a0
	movl	sp,d0
	jmp	a0@
