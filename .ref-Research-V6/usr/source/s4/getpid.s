/ getpid -- get process ID

getpid	= 20.

.globl	_getpid

_getpid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	getpid
	mov	(sp)+,r5
	rts	pc

