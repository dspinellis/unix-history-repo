	.globl	_syscall
	.globl	cerror
_syscall:
	.word	0x0000
	movl	4(ap),r0		# syscall number
	subl3	$1,(ap)+,(ap)	# one fewer arguments
	chmk	r0				# do it
	bcs		L1
	ret
L1:
	jmp		cerror
