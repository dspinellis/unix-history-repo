.globl	_sleep, retrn
sleep = 35.

_sleep:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	sys	sleep
	jmp	retrn
