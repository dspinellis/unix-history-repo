/
/

/ amod & dmod fortran functions

.globl	amod.
.globl	dmod.

.globl	retrn
one = 40200
.globl	temp
.globl	rerr

dmod.:	temp
	.+2
	setd
	br	1f

amod.:	temp
	.+2
	setf
1:
	cmp	*2(sp),$2
	bne	1f
	movf	*2(r3),r0
	movf	r0,r2
	movf	*4(r3),r1
	divf	r1,r2
	modf	$one,r2
	mulf	r1,r3
	subf	r3,r0
	movf	r0,temp
	jmp	retrn

1:
	jsr	r5,rerr; 2
