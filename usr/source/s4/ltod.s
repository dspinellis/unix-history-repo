/ C library
/ return floating-point from long integer
/	d = ltod(l)

.globl	_ltod
_ltod:
	mov	r5,-(sp)
	mov	sp,r5
	setl
	movif	*4(r5),fr0
	seti
	mov	(sp)+,r5
	rts	pc

/ return long integer from floating
/	dtol(d, l)

.globl	_dtol
_dtol:
	mov	r5,-(sp)
	mov	sp,r5
	setl
	movf	4(r5),fr0
	movfi	fr0,*12.(r5)
	seti
	mov	(sp)+,r5
	rts	pc
