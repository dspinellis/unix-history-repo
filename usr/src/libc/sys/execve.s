/ C library -- execve

/ execve(file, argv, env);
/
/ where argv is a vector argv[0] ... argv[x], 0
/ last vector element must be 0

.globl	_execve
.globl	cerror
.exece	= 59.

_execve:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	mov	6(r5),0f+2
	mov	8(r5),0f+4
	sys	0; 9f
	jmp	cerror
.data
9:
	sys	.exece; 0:..; ..; ..
