/
/

/ cabs fortran function

.globl	cabs.
.globl	dcabs.

.globl	sqrt
.globl	retrn
.globl	rerr
.globl	temp

dcabs.:	temp
	.+2
	setd
	br	1f

cabs.:	temp
	.+2
	setf
1:
	mov	2(r3),r1
	movf	(r1)+,r0
	movf	(r1)+,r1
	jsr	pc,hypot
	bes	1f
	movf	r0,temp
	jmp	retrn
1:
	jsr	r5,rerr; 4
hypot:
	movf	fr1,-(sp)
	absf	fr0
	absf	fr1
	cmpf	fr0,fr1
	cfcc
	bne	1f
	movf	$one,fr0
	br	3f
1:
	blt	2f
	movf	fr0,-(sp)
	movf	fr1,fr0
	movf	(sp)+,fr1
2:
	divf	fr1,fr0
	mulf	fr0,fr0
3:
	addf	$one,fr0
	jsr	pc,sqrt
	mulf	fr1,fr0
	cfcc
	movf	(sp)+,fr1
	rts	pc

one = 40200
