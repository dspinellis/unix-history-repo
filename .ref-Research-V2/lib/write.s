/ C library -- write

/ nwritten = write(file, buffer, count);
/
/ nwritten == -1 means error

	.globl	_write

.data
_write:
	1f
.text
1:
	mov	2(sp),r0
	mov	4(sp),0f
	mov	6(sp),0f+2
	sys	write; 0:..; ..
	bec	1f
	mov	$-1,r0
1:
	rts	pc

