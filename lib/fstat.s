/ C library -- fstat

/ error = fstat(file, statbuf);

/ int statbuf[17] or
/ char statbuf[34]
/ as appropriate

	.globl	_fstat

.data
_fstat:
	1f
.text
1:
	mov	2(sp),r0
	mov	4(sp),0f
	sys	fstat; 0:..
	bec	1f
	mov	$1,r0
	rts	pc
1:
	clr	r0
	rts	pc

