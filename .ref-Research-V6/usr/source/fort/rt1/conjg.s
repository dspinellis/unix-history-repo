/
/

/ conjg fortran function

.globl	conjg.

.globl	rval8p
.globl	retrn
.globl	temp

conjg.:	temp
	rval8p; 2
	.+2
	setf
	movf	(sp)+,r0
	movf	r0,temp
	movf	(sp)+,r0
	negf	r0
	movf	r0,temp+4
	jmp	retrn
