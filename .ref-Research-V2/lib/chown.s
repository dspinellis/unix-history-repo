/ C library -- chown

/ error = chown(string, owner);

	.globl	_chown

.data
_chown:
	1f
.text
1:
	mov	2(sp),0f
	mov	4(sp),0f+2
	clr	r0
	sys	chown; 0:..; ..
	adc	r0
	rts	pc

