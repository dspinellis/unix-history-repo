/ C library -- setuid

/ error = setuid(uid);

.globl	_setuid, cerror

_setuid:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	sys	setuid
	bec	1f
	jmp	cerror
1:
	clr	r0
	mov	(sp)+,r5
	rts	pc
