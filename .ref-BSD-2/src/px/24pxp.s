/
/ PXP stuff
/
_PXPBUF:
	mov	(lc),r0
	mov	r0,r2
	add	$3,r0
	add	r0,r0
	add	r0,r0
	mov	r0,-(sp)
	mov	r0,_pxpsize
	jsr	pc,_alloc
	mov	r0,_pxpbuf
	mov	r0,r2
	mov	$426,(r2)+
	clr	(r2)+
	sys	time
	mov	r0,(r2)+
	mov	r1,(r2)+
	mov	(lc)+,(r2)+
	tst	(lc)+
	mov	(lc)+,(r2)+
	return
_TRACNT:
	mov	4(lc),r3
	add	(lc),lc
	br	1f
_COUNT:
	bne	1f
	mov	(lc)+,r3
1:
	add	r3,r3
	add	r3,r3
	add	_pxpbuf,r3
	add	$1,2(r3)
	adc	(r3)
	return
