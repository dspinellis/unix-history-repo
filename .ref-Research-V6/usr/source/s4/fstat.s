/ C library -- fstat

/ error = fstat(file, statbuf);

/ int statbuf[17] or
/ char statbuf[34]
/ as appropriate

.globl	_fstat, cerror

_fstat:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	mov	6(r5),0f
	sys	0; 9f
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc
.data
9:
	sys	fstat; 0:..
