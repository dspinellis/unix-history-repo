/ C library -- execv

/ execv(file, argv);
/
/ where argv is a vector argv[0] ... argv[x], 0
/ last vector element must be 0
/ environment passed automatically

.globl	_execv,
.globl	cerror, _environ
.exece	= 59.

_execv:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	mov	6(r5),0f+2
	mov	_environ,0f+4
	sys	0; 9f
	jmp	cerror
.data
9:
	sys	.exece; 0:..; ..; ..
