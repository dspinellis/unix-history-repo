/
/ ADDITION
/
_ADD42:
	mov	(sp)+,r0
	mov	(sp)+,r1
	br	1f
_ADD2:
	mov	(sp)+,r1
	sxt	r0
1:
	mov	(sp)+,r3
	sxt	r2
	add	r1,r3
	adc	r2
	add	r0,r2
	mov	r3,-(sp)
	mov	r2,-(sp)
	return
_ADD24:
	mov	(sp)+,r1
	sxt	r0
	add	r1,2(sp)
	adc	r0
	add	r0,(sp)
	return
_ADD4:
	add	(sp)+,2(sp)
	add	(sp)+,2(sp)
	adc	(sp)
	return
_ADD8:
	movf	(sp)+,fr0
	addf	(sp)+,fr0
	cfcc
	bvs	9f
	movf	fr0,-(sp)
	return
_ADD28:
	tst	(sp)
	sxt	-(sp)
_ADD48:
	movif	(sp)+,fr0
	addf	(sp)+,fr0
	cfcc
	bvs	9f
	movf	fr0,-(sp)
	return
_ADD82:
	movf	(sp)+,fr0
	tst	(sp)
	sxt	-(sp)
	br	1f
_ADD84:
	movf	(sp)+,fr0
1:
	movif	(sp)+,fr2
	addf	fr0,fr2
	cfcc
	bvs	9f
	movf	fr2,-(sp)
	return
9:
fpovflo:
	mov	$EFPOVFLO,_perrno
	error	EFPOVFLO
