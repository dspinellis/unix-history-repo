/ C library -- chmod

/ error = chmod(string, mode);

	.globl	_chmod

.data
_chmod:
	1f
.text
1:
	mov	2(sp),0f
	mov	4(sp),0f+2
	clr	r0
	sys	chmod; 0:..; ..
	adc	r0
	rts	pc

