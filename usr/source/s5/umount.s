/ C library -- umount/

.globl	_umount
.globl	cerror
indir	= 0
.comm	_errno,2

_umount:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(sp),0f
	sys	indir; 9f
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc

.data
9:
	sys	umount; 0:..
