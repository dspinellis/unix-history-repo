/ C library -- read

/ nread = read(file, buffer, count);
/
/ nread ==0 means eof; nread == -1 means error

.globl	_read, cerror

_read:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	mov	6(r5),0f
	mov	8(r5),0f+2
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	mov	(sp)+,r5
	rts	pc
.data
9:
	sys	read; 0:..; ..
