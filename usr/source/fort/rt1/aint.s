/
/

/ aint fortran function

.globl	aint.
.globl	temp

one = 40200

.globl	retrn

aint.:	temp
	.+2
	setf
	movf	*2(r3),r0
	modf	$one,r0
	movf	r1,temp
	jmp	retrn
