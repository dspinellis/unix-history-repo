.globl	_sync
sync = 36.

_sync:
	mov	r5,-(sp)
	mov	sp,r5
	sys	sync
	mov	(sp)+,r5
	rts	pc
