/ C library -- execv

/ execv(file, argv);
/
/ where argv is a vector argv[0] ... argv[x], 0
/ last vector element must be 0
/

.globl	_execv, cerror

_execv:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	mov	6(r5),0f+2
	sys	0; 9f
	jmp	cerror
.data
9:
	sys	exec; 0:..; ..
