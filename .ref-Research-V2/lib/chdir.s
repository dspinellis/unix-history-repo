/ C library -- chdir

/ error = chdir(string);

	.globl	_chdir

.data
_chdir:
	1f
.text
1:
	mov	2(sp),0f
	clr	r0
	sys	chdir; 0:..
	adc	r0
	rts	pc

