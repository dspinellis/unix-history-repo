/ C library -- unlink

/ error = unlink(string);
/

	.globl	_unlink

.data
_unlink:
	1f
.text
1:
	mov	2(sp),0f
	clr	r0
	sys	unlink; 0:..
	adc	r0
	rts	pc

