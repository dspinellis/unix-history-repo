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
	tstl	r1		# child process ?
	bneq	child	# yes
	bcc 	parent		# if c-bit not set, fork ok
	jmp 	cerror
child:
	clrl	r0
parent:
	ret
