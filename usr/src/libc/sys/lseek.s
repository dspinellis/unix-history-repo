/ C library -- lseek

/ error = lseek(file, offset, ptr);
/ long offset
/ long lseek()

.globl	_lseek, cerror
.lseek	= 19.

_lseek:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	mov	6(r5),0f
	mov	8(r5),0f+2
	mov	10.(r5),0f+4
	sys	0; 9f
	bec	1f
	mov	$1,r1
	jmp	cerror
1:
	mov	(sp)+,r5
	rts	pc
.data
9:
	sys	.lseek; 0:..; ..; ..
