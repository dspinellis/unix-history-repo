/ C library -- close

/ error =  close(file);

.globl	_close, cerror

_close:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	sys	close
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc
