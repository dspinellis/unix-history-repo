.globl	_stime, retrn, cerror

_stime:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(sp),r1
	mov	(r1)+,r0
	mov	(r1),r1
	sys	stime
	bec	1f
	jmp	cerror
1:
	clr	r0
	jmp	retrn
