/
/

/ min1, amin1 & dmin1 fortran functions

.globl	amin1.
.globl	dmin1.
.globl	min1.

.globl	retrn
.globl	temp

min1.:	temp
	.+2
	setl
	setf
	mov	pc,r2
	br	2f

dmin1.:	temp
	.+2
	setd
	br	1f

amin1.:	temp
	.+2
	setf
1:
	clr	r2
2:
	mov	*2(sp),r0		/ arg count
	mov	r3,r1
	tst	(r1)+			/ argpp
	movf	*(r1)+,r1
	br	2f
1:
	movf	*(r1)+,r0
	cmpf	r0,r1
	cfcc
	bge	2f
	movf	r0,r1
2:
	dec	r0
	bgt	1b
	tst	r2
	bne	1f
	movf	r1,temp
	br	2f
1:
	movfi	r1,temp
2:
	jmp	retrn
