/ C library -- kill

.globl	_kill, retrn, cerror
kill = 37.
indir = 0

_kill:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(sp),r0
	mov	6(sp),8f
	sys	indir; 9f
	.data
9:	sys	kill; 8:..
	.text
	bec	1f
	jmp	cerror
1:
	clr	r0
	jmp	retrn
