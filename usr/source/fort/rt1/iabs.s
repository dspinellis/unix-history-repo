/
/

/ iabs fortran function

.globl	iabs.

.globl	retrn
.globl	temp

iabs.:	temp
	.+2
	setl
	setd
	movif	*2(r3),r0
	absf	r0
	movfi	r0,temp
	jmp	retrn
