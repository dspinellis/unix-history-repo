/ C library -- getgid

/ gid = getgid();
/

getgid = 47.
.globl	_getgid

_getgid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	getgid
	mov	(sp)+,r5
	rts	pc
