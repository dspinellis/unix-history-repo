/ C library -- getgid, getegid

/ gid = getgid();

.globl	_getgid
.getgid = 47.

_getgid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	.getgid
	mov	(sp)+,r5
	rts	pc

/ gid = getegid();
/ returns effective gid

.globl	_getegid

_getegid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	.getgid
	mov	r1,r0
	mov	(sp)+,r5
	rts	pc
