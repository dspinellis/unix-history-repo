/ C library -- makdir

/ error = makdir(string);

	.globl	_makdir

.data
_makdir:
	1f
.text
1:
	mov	2(sp),0f
	clr	r0
	sys	makdir; 0:..
	adc	r0
	rts	pc

