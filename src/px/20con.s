/
/ CONOPS
/
_CON1:
	mov	r3,-(sp)
	return
_CON2:
	mov	(lc)+,-(sp)
	return
_CON4:
	mov	(lc)+,r0
	mov	(lc)+,-(sp)
	mov	r0,-(sp)
	return
_CON8:
	mov	(lc)+,r0
	mov	(lc)+,r1
	mov	(lc)+,r2
	mov	(lc)+,-(sp)
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,-(sp)
	return
_CON:
	bne	1f
	mov	(lc)+,r3
1:
	mov	r3,r2
	inc	r2
	bic	$1,r2
	mov	lc,r0
	add	r2,lc
	br	_INDcon
