/ C library -- execl

/ execl(file, arg1, arg2, ... , 0);
/
/
	.globl	_execl

.data
_execl:
	1f
.text
1:
	mov	2(sp),0f
	mov	sp,r0
	add	$4,r0
	mov	r0,0f+2
	sys	exec; 0:..; ..
	rts	pc

