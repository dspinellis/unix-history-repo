.globl	_sync, retrn
sync = 36.

_sync:
	mov	r5,-(sp)
	mov	sp,r5
	sys	sync
	jmp	retrn
