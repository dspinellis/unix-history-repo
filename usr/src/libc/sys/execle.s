/ C library -- execle

/ execle(file, arg1, arg2, ... , 0, env);
/

.globl	_execle
.globl	cerror
.exece	= 59.

_execle:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),0f
	mov	r5,r0
	add	$6,r0
	mov	r0,0f+2
1:
	tst	(r0)+
	bne	1b
	mov	(r0),0f+4
	sys	0; 9f
	jmp	cerror
.data
9:
	sys	.exece; 0:..; ..; ..
