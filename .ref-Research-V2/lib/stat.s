/ C library -- stat

/ error = stat(string, statbuf);

/ int statbuf[17] or
/ char statbuf[34]
/ as appropriate

	.globl	_stat

.data
_stat:
	1f
.text
1:
	mov	2(sp),0f
	mov	4(sp),0f+2
	sys	stat; 0:..; ..
	bec	1f
	mov	$1,r0
	rts	pc
1:
	clr	r0
	rts	pc

