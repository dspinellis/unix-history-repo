/
/ OFF, INDEX, LV and NIL
/
_OFF:
	bne	1f
	mov	(lc)+,r3
1:
	add	r3,(sp)
	return
_INX4:
	tst	(sp)+		/ should check bounds here
	tst	r3
_INX2:
	bne	1f
	mov	(lc)+,r3
1:
	mov	(sp)+,r1
	sub	(lc)+,r1
	bmi	1f
	cmp	r1,(lc)+
	bgt	1f
	mul	r3,r1
	add	r1,(sp)
	return
1:
	mov	$ESUBSCR,_perrno
	error	ESUBSCR
_NIL:
	tst	(sp)
	beq	1f
	return
1:
	mov	$ENILPTR,_perrno
	error	ENILPTR
_LV:
	mov	_display(r3),r0
	add	(lc)+,r0
	mov	r0,-(sp)
	return
_INX4P2:
	tst	(sp)+
_INX2P2:
	mov	(sp)+,r0
	sub	(lc)+,r0
	ash	r3,r0
	add	r0,(sp)
	return
