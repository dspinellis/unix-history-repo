/ C library -- getuid

/ uid = getuid();
/

.globl	_getuid

_getuid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	getuid
	mov	(sp)+,r5
	rts	pc
