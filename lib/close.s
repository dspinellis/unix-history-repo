/ C library -- close

/error =  close(file);

	.globl	_close

.data
_close:
	1f
.text
1:
	mov	2(sp),r0
	clr	r1
	sys	close
	adc	r1
	mov	r1,r0
	rts	pc

