/ C library-- nice

/ error = nice(hownice)

.globl	_nice, cerror

_nice:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(sp),r0
	sys	nice
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc
