/ C library -- _exit

/ _exit(code)
/ code is return in r0 to system
/ Same as plain exit, for user who want to define their own exit.

.globl	__exit
.exit = 1.

__exit:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	sys	.exit

