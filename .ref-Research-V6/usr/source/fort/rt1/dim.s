/
/

/ dim fortran function

.globl	dim.

.globl	retrn
.globl	temp
.globl	rerr
/
dim.:	temp
	.+2
	setf
	cmp	*2(sp),$2
	bne	2f
	movf	*2(r3),r0
	subf	*4(r3),r0
	cfcc
	bge	1f
	clrf	r0
1:
	movf	r0,temp
	jmp	retrn

2:
	jsr	r5,rerr; 7.
