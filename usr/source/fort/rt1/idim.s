/
/

/ idim fortran function

.globl	idim.

.globl	retrn
.globl	rerr
.globl	temp

idim.:	temp
	.+2
	setd
	setl
	cmp	*2(sp),$2
	bne	2f
	movif	*2(r3),r0
	movif	*4(r3),r1
	subf	r1,r0
	cfcc
	bge	1f
	clrf	r0
1:
	movfi	r0,temp
	jmp	retrn

2:
	jsr	r5,rerr; 9.
