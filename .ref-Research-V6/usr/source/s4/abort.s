/ C library -- abort

.globl	_abort
iot	= 4

_abort:
	mov	r5,-(sp)
	mov	sp,r5
	iot
	mov	(sp)+,r5
	rts	pc
