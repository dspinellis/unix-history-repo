# @(#)ptrace.s	4.1 (Berkeley) 12/21/80
# ptrace -- C library

#	result = ptrace(req, pid, addr, data);

	.set	ptrace,26
.globl	_ptrace
.globl	cerror
.globl  _errno

_ptrace:
	.word	0x0000
	clrl	_errno
	chmk	$ptrace
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
