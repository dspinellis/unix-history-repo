# @(#)setuid.s	4.1 (Berkeley) 12/21/80
# C library -- setuid

# error = setuid(uid);

	.set	setuid,23
.globl	_setuid
.globl  cerror

_setuid:
	.word	0x0000
	chmk	$setuid
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
