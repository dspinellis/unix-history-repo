# C library -- mknod

# error = mknod(string, mode, major.minor);

	.set	mknod,14
.globl	_mknod
.globl  cerror

_mknod:
	.word	0x0000
	chmk	$mknod
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
