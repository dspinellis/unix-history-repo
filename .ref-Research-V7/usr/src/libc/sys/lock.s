/ lock -- C library

/	lock(f)

.globl	_lock, cerror

.lock = 53.

_lock:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	sys	0; 9f
	.data
9:
	sys	.lock; 0:..
	.text
	bec	1f
	jmp	cerror
1:
	mov	(sp)+,r5
	rts	pc
