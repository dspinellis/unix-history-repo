/
/ RVALUES
/
_RV2:
	mov	_display(r3),r0
	add	(lc)+,r0
	mov	(r0),-(sp)
	return
_RV4:
	mov	_display(r3),r0
	add	(lc)+,r0
	sub	$4,sp
	mov	sp,r2
	mov	(r0)+,(r2)+
	mov	(r0)+,(r2)+
	return
_RV1:
	mov	_display(r3),r0
	add	(lc)+,r0
	movb	(r0),r1
	mov	r1,-(sp)
	return
_RV8:
	mov	_display(r3),r0
	add	(lc)+,r0
	sub	$8,sp
	mov	sp,r2
	mov	(r0)+,(r2)+
	mov	(r0)+,(r2)+
	mov	(r0)+,(r2)+
	mov	(r0)+,(r2)+
	return
_RV:
	mov	_display(r3),r0
	add	(lc)+,r0
	mov	r0,-(sp)
	clr	r3
	br	_IND
