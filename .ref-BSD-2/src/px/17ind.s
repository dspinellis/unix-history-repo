/
/ INDS
/
_IND2:
	mov	*(sp)+,-(sp)
	return
_IND4:
	mov	(sp),r0
	tst	-(sp)
	mov	sp,r2
	mov	(r0)+,(r2)+
	mov	(r0)+,(r2)+
	return
_IND1:
	movb	*(sp),r0
	mov	r0,(sp)
	return
_IND8:
	mov	$8,r3
_IND:
	bne	1f
	mov	(lc)+,r3
1:
	mov	(sp)+,r0
_INDcon:
	add	r3,r0
	asr	r3
	bcc	1f
	movb	-(r0),r1
	bic	$!377,r1
	mov	r1,-(sp)		/ clean for sets, e.g.
1:
	inc	r3
	br	2f
1:
	mov	-(r0),-(sp)
2:
	sob	r3,1b
	return
