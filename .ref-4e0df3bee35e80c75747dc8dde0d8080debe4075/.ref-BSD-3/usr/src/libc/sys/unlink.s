# C library -- unlink

# error = unlink(string);
#

	.set	unlink,10
.globl	_unlink
.globl  cerror

_unlink:
	.word	0x0000
	chmk	$unlink
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
