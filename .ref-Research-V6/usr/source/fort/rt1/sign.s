/
/

/ sign & dsign fortran functions

.globl	sign.
.globl	dsign.

.globl	retrn
.globl	temp
.globl	rerr

dsign.:	temp
	.+2
	setd
	br	1f

sign.:	temp
	.+2
	setf
1:
	cmp	*2(sp),$2
	bne	2f
	movf	*2(r3),r0
	absf	r0
	tstf	*4(r3)
	cfcc
	bge	1f
	negf	r0
1:
	movf	r0,temp
	jmp	retrn

2:
	jsr	r5,rerr; 12.
