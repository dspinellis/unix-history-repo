/ C library -- kill

.globl	_kill, cerror
kill = 37.
indir = 0

_kill:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(sp),r0
	mov	6(sp),8f
	sys	indir; 9f
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc

.data
9:
	sys	kill; 8:..
