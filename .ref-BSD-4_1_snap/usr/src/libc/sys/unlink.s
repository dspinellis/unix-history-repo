# @(#)unlink.s	4.1 (Berkeley) 12/21/80
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
