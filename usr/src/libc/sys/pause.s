/ C library - pause

.globl	_pause
.pause = 29.

_pause:
	mov	r5,-(sp)
	mov	sp,r5
	sys	.pause
	mov	(sp)+,r5
	rts	pc
