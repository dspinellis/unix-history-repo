# @(#)chown.s	4.1 (Berkeley) 12/21/80
# C library -- chown

# error = chown(string, owner);

	.set	chown,16
.globl	_chown

_chown:
	.word	0x0000
	chmk	$chown
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
