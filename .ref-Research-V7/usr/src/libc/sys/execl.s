/ C library -- execl

/ execl(file, arg1, arg2, ... , 0);
/
/ environment automatically passed

.globl	_execl
.globl	cerror, _environ
.exece	= 59.

_execl:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	mov	r5,r0
	add	$6,r0
	mov	r0,0f+2
	mov	_environ,0f+4
	sys	0; 9f
	jmp	cerror
.data
9:
	sys	.exece; 0:..; ..; ..
