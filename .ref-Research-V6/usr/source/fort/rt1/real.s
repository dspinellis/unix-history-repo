/
/

/ real & dreal fortran function

.globl	real.
.globl	dreal.

.globl	retrn
.globl	temp

dreal.:	temp
	.+2
	setd
	br	1f

real.:	temp
	.+2
	setf
1:
	movf	*2(r3),r0
	movf	r0,temp
	jmp	retrn
