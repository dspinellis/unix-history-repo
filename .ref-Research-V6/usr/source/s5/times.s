/ C library -- times

.globl	_times
times	= 43.

_times:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	sys	0; 9f
	mov	(sp)+,r5
	rts	pc
.data
9:
	sys	times; 0:..
