/ C library -- seek

/ error = seek(file, offset, ptr);

	.globl	_seek

.data
_seek:
	1f
.text
1:
	mov	2(sp),r0
	mov	4(sp),0f
	mov	6(sp),0f+2
	sys	seek; 0:..; ..
	bec	1f
	mov	$1,r0
	rts	r0
1:
	clr	r0
	rts	pc


