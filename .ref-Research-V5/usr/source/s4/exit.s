/ C library -- exit

/ exit(code)
/ code is return in r0 to system

.globl	_exit

_exit:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	sys	exit

