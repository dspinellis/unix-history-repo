/
/

/ dimag fortran function

.globl	dimag.

.globl	rval16p
.globl	retrn
.globl	temp

dimag.:	temp
	rval16p; 2
	.+2
	setd
	tstf	(sp)+
	movf	(sp)+,r0
	movf	r0,temp
	jmp	retrn
