# @(#)setgid.s	4.1 (Berkeley) 12/21/80
# C library -- setgid

# error = setgid(uid);

	.set	setgid,46
.globl	_setgid
.globl  cerror

_setgid:
	.word	0x0000
	chmk	$setgid
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
