# @(#)chdir.s	4.1 (Berkeley) 12/21/80
# C library -- chdir

# error = chdir(string);

	.set	chdir,12
.globl	_chdir

_chdir:
	.word	0x0000
	chmk	$chdir
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
