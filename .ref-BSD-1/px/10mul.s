/
/ MULTIPLICATION & SQUARING
/
_SQR2:
	mov	(sp),-(sp)
_MUL2:
	mov	(sp)+,r0
	mul	(sp)+,r0
	mov	r1,-(sp)
	mov	r0,-(sp)
	return
_MUL42:
	mov	2(sp),r2
	sxt	r1
	sub	(sp),r1 
	tst	4(sp)
	sxt	2(sp)
	br	1f
_MUL24:
	tst	(sp)
	sxt	-(sp)
	br	_MUL4
_SQR4:
	mov	2(sp),-(sp)
	mov	2(sp),-(sp)
_MUL4:
	mov	2(sp),r2
	sxt	r1
	sub	(sp)+,r1
1:
	mov	4(sp),r0
	sxt	r3
	sub	2(sp),r3
	mul	r0,r1
	mul	r2,r3
	add	r1,r3
	mul	r2,r0
	sub	r3,r0
	add	$6.,sp
	mov	r1,-(sp)
	mov	r0,-(sp)
	return
_SQR8:
	movf	(sp),fr0
	br	1f
_MUL8:
	movf	(sp)+,fr0
1:
	mulf	(sp)+,fr0
	movf	fr0,-(sp)
	return
_MUL28:
	tst	(sp)
	sxt	-(sp)
_MUL48:
	movif	(sp)+,fr0
	mulf	(sp)+,fr0
	movf	fr0,-(sp)
	return
_MUL82:
	movf	(sp)+,fr0
	tst	(sp)
	sxt	-(sp)
	br	1f
_MUL84:
	movf	(sp)+,fr0
1:
	movif	(sp)+,fr2
	mulf	fr0,fr2
	movf	fr2,-(sp)
	return
