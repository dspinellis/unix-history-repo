/
/

/ max1, amax1 & dmax1 fortran functions

.globl	amax1.
.globl	dmax1.
.globl	max1.

.globl	retrn
.globl	temp

max1.:	temp
	.+2
	setl
	setf
	mov	pc,r2		/ integer flag
	br	2f

dmax1.:	temp
	.+2
	setd
	br	1f

amax1.:	temp
	.+2
	setf
1:
	clr	r2		/ integer flag
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
