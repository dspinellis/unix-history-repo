/
/

/ max0 & amax0 fortran functions

.globl	amax0.
.globl	max0.

.globl	retrn
.globl	temp

max0.:	temp
	.+2
	mov	pc,r2
	br	1f

amax0.:	temp
	.+2
	clr	r2
1:
	setf
	setl
	mov	*2(sp),r0		/ arg count
	mov	r3,r1
	tst	(r1)+			/ argpp
	movif	*(r1)+,r1
	br	2f
1:
	movif	*(r1)+,r0
	cmpf	r0,r1
	cfcc
	ble	2f
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
