/
/

/ abs & dabs fortran functions

.globl	abs.
.globl	dabs.

.globl	retrn
.globl	temp

dabs.:	temp
	.+2
	setd
	br	1f

abs.:	temp
	.+2
	setf
1:
	movf	*2(r3),r0
	absf	r0
	movf	r0,temp
	jmp	retrn
