/ C library -- seek

/ error = seek(file, offset, ptr);

.globl	_seek, retrn, cerror

_seek:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	mov	6(r5),0f
	mov	8(r5),0f+2
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	clr	r0
	jmp	retrn
.data
9:
	sys	seek; 0:..; ..
