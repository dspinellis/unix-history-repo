/
/

/ aimag fortran function

.globl	aimag.

.globl	rval8p
.globl	retrn
.globl	temp

aimag.:	temp
	rval8p; 2
	.+2
	setf
	tstf	(sp)+
	movf	(sp)+,r0
	movf	r0,temp
	jmp	retrn
