/
/

/ sqrt & dsqrt fortran functions

.globl	sqrt.
.globl	dsqrt.

.globl	sqrt
.globl	retrn
.globl	rerr
.globl	temp

dsqrt.:	temp
	.+2
	setd
	br	1f

sqrt.:	temp
	.+2
	setf
1:
	movf	*2(r3),r0
	jsr	pc,sqrt
	bes	1f
	movf	r0,temp
	jmp	retrn
1:
	jsr	r5,rerr; 13.
