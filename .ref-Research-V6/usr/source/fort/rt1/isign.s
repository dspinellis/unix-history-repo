/
/

/ isign fortran function

.globl	isign.

.globl	retrn
.globl	rerr
.globl	temp

isign.:	temp
	.+2
	setd
	setl
	cmp	*2(sp),$2
	bne	2f
	movif	*2(r3),r0
	absf	r0
	movif	*4(r3),r1
	cfcc
	bge	1f
	negf	r0
1:
	movfi	r0,temp
	jmp	retrn

2:
	jsr	r5,rerr; 10.
