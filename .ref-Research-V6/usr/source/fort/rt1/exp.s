/
/

/ exp & dexp fortran functions

.globl	exp.
.globl	dexp.

.globl	exp
.globl	retrn
.globl	rerr
.globl	temp

dexp.:	temp
	.+2
	setd
	br	1f

exp.:	temp
	.+2
	setf
1:
	seti
	movf	*2(r3),r0
	jsr	pc,exp
	bes	1f
	movf	r0,temp
	jmp	retrn

1:
	jsr	r5,rerr; 8.
