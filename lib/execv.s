/ C library -- execv

/ execv(file, argv);
/
/ where argv is a vector argv[0] ... argv[x], 0
/ last vector element must be 0
/
	.globl	_execv

.data
_execv:
	1f
.text
1:
	mov	2(sp),0f
	mov	4(sp),0f+2
	sys	exec; 0:..; ..
	rts	pc

