# pipe -- C library

#	pipe(f)
#	int f[2];

	.set	pipe,42
.globl	_pipe
.globl  cerror

_pipe:
	.word	0x0000
	chmk	$pipe
	bcc 	noerror
	jmp 	cerror
noerror:
	movl	4(ap),r2
	movl	r0,(r2)+
	movl	r1,(r2)
	clrl	r0
	ret
