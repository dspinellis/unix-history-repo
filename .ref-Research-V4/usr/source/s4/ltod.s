/ C library -- return floating-point from long integer

.globl	_ltod
_ltod:
	mov	r5,-(sp)
	mov	sp,r5
	setl
	movif	*4(r5),fr0
	seti
	mov	(sp)+,r5
	rts	pc
