/ C library -- umount/

.globl	_umount, retrn
.comm	_errno,2

.data
_umount:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(sp),0f
	sys	umount; 0:..
	bes	1f
	clr	r0
	br	2f
1:
	mov	r0,_errno
	mov	$-1,r0
2:
	jmp	retrn
