/ C library - getcsw

/ csw = getcsw();

.globl	_getcsw

_getcsw:
	mov	r5,-(sp)
	mov	sp,r5
	sys	38.
	mov	(sp)+,r5
	rts	pc
