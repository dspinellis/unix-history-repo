/ C library -- read

/ nread = read(file, buffer, count);
/
/ nread ==0 means eof; nread == -1 means error

	.globl	_read

.data
_read:
	1f
.text
1:
	mov	2(sp),r0
	mov	4(sp),0f
	mov	6(sp),0f+2
	sys	read; 0:..; ..
	bec	1f
	mov	$-1,r0
1:
	rts	pc

