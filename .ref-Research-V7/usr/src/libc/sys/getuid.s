/ C library -- getuid, geteuid

/ uid = getuid();

.globl	_getuid
.getuid = 24.

_getuid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	.getuid
	mov	(sp)+,r5
	rts	pc


/ uid = geteuid();
/  returns effective uid

.globl	_geteuid

_geteuid:
	mov	r5,-(sp)
	mov	sp,r5
	sys	.getuid
	mov	r1,r0
	mov	(sp)+,r5
	rts	pc
