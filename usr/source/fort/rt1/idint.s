/
/

/ idint fortran function

.globl	idint.

.globl	retrn
.globl	temp

idint.:	temp
	.+2
	setd
	setl
	movf	*2(r3),r0
	movfi	r0,temp
	jmp	retrn
