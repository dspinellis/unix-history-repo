/
/ SUBTRACTION
/
_SUB42:
	mov	(sp)+,r0
	mov	(sp)+,r1
	br	1f
_SUB2:
	mov	(sp)+,r1
	sxt	r0
1:
	mov	(sp)+,r3
	sxt	r2
	sub	r1,r3
	sbc	r2
	sub	r0,r2
	mov	r3,-(sp)
	mov	r2,-(sp)
	return
_SUB24:
	mov	(sp)+,r1
	sxt	r0
	sub	r1,2(sp)
	adc	r0
	sub	r0,(sp)
	return
_SUB4:
	sub	(sp)+,2(sp)
	sub	(sp)+,2(sp)
	sbc	(sp)
	return
_SUB8:
	movf	(sp)+,fr0
	movf	(sp)+,fr2
	subf	fr0,fr2
	cfcc
	bvs	9f
	movf	fr2,-(sp)
	return
_SUB28:
	tst	(sp)
	sxt	-(sp)
_SUB48:
	movif	(sp)+,fr0
	movf	(sp)+,fr2
	subf	fr0,fr2
	cfcc
	bvs	9f
	movf	fr2,-(sp)
	return
_SUB82:
	movf	(sp)+,fr0
	tst	(sp)
	sxt	-(sp)
	br	1f
_SUB84:
	movf	(sp)+,fr0
1:
	movif	(sp)+,fr2
	subf	fr0,fr2
	cfcc
	bvs	9f
	movf	fr2,-(sp)
	return
9:
	jmp	fpovflo
