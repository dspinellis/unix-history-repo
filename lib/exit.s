/ C library -- exit

/ exit(code)
/ code is return in r0 to system

.globl	_exit

.data
_exit:
	1f
.text
1:
	mov	2(sp),r0
	sys	exit

