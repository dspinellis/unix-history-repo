/
/

/ atan2 & datan2 fortran functions

.globl	atan2.
.globl	datan2.

.globl	retrn
.globl	rerr
.globl	temp
.globl	atan2

datan2.:	temp
	.+2
	setd
	br	1f

atan2.:	temp
	.+2
	setf
1:
	cmp	*2(sp),$2
	bne	1f
	movf	*2(r3),r0
	movf	*4(r3),r1
	jsr	pc,atan2
	movf	r0,temp
	jmp	retrn
1:
	jsr	r5,rerr; 3
