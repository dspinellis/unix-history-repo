/
/

/ sin & dsin fortran functions

.globl	sin.
.globl	dsin.

.globl	sin
.globl	retrn
.globl	temp

dsin.:	temp
	.+2
	setd
	br	1f

sin.:	temp
	.+2
	setf
1:
	movf	*2(r3),r0
	jsr	pc,sin
	movf	r0,temp
	jmp	retrn
