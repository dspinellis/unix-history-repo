	.globl	_vsyscall
	.globl	cerror
_vsyscall:
	.word	0x0000
	movl	4(ap),r0		# point at arg array
	movl	(r0)+,r1		# syscall number	
	movl	$10,r2
Loop:
	pushl	(r0)[r2]			# push argument
	sobgeq	r2,Loop			# push 10 arguments	
	pushl	$10				# arg count
	movl	sp,ap			# point at args	
	chmk	r1				# do it
	bcs		L1
	ret
L1:
	jmp		cerror
