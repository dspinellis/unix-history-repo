/ C library -- mount

/ error = mount(dev, file, flag)

.globl	_mount, cerror

_mount:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(sp),0f
	mov	6(sp),0f+2
	mov	8(sp),0f+4
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc
.data
9:
	sys	mount; 0:..; ..; ..
