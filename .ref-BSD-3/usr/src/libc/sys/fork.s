# C library -- fork

# pid = fork();
#
# r1 == 0 in parent process, r1 == 1 in child process.
# r0 == pid of child in parent, r0 == pid of parent in child.

	.set	fork,2
.globl	_fork

_fork:
	.word	0x0000
	chmk	$fork
	bcc		forkok
	jmp		cerror
forkok:
	jlbc	r1,parent
	clrl	r0		# signify child
parent:
	ret
