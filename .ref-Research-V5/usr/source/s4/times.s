/ C library -- times

.globl	_times, retrn
times	= 43.

_times:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	sys	0; 9f
	jmp	retrn
.data
9:
	sys	times; 0:..
