/ C library -- exit

/ exit(code)
/ code is return in r0 to system

.globl	_exit
.globl	__cleanup
exit = 1

_exit:
	mov	r5,-(sp)
	mov	sp,r5
	jsr	pc,__cleanup
	mov	4(r5),r0
	sys	exit

