/
/

/ mod fortran function

.globl	mod.

.globl	retrn
.globl	temp
one = 40200
.globl	rerr

mod.:	temp
	.+2
	setd
	setl
	cmp	*2(sp),$2
	bne	1f
	movif	*2(r3),r0
	movf	r0,r2
	movif	*4(r3),r1
	divf	r1,r2
	modf	$one,r2
	mulf	r1,r3
	subf	r3,r0
	movfi	r0,temp
	jmp	retrn

1:
	jsr	r5,rerr; 11.
