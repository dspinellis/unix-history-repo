/ C library -- link

/ error = link(old-file, new-file);
/

	.globl	_link

.data
_link:
	1f
.text
1:
	mov	2(sp),0f
	mov	4(sp),0f+2
	clr	r0
	sys	link; 0:..; ..
	adc	r0
	rts	pc

