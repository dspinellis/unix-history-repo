/ C library -- dup

/	f = dup(of)
/	f == -1 for error

dup = 41.

.globl	_dup, cerror

_dup:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	sys	dup
	bec	1f
	jmp	cerror
1:
	mov	(sp)+,r5
	rts	pc
