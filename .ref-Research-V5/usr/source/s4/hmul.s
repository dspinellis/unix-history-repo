.globl	_hmul

_hmul:
	mov	2(sp),r0
	mul	4(sp),r0
	rts	pc
