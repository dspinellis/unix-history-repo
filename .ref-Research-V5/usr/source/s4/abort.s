/ C library -- abort

.globl	_abort

_abort:
	mov	r5,-(sp)
	mov	sp,r5
	4
