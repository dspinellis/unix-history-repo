# @(#)execve.s	4.1 (Berkeley) 12/21/80
# C library -- execve

# execve(file, argv, env);
#
# where argv is a vector argv[0] ... argv[x], 0
# last vector element must be 0

	.set	exece,59
.globl	_execve

_execve:
	.word	0x0000
	chmk	$exece
	jmp 	cerror
