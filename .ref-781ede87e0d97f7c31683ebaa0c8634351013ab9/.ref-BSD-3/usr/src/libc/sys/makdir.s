# C library -- makdir

# error = makdir(string);

	.set	mknod,14
.globl	_makdir
.globl  cerror

_makdir:
	.word	0x0000
	chmk	$mknod
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
