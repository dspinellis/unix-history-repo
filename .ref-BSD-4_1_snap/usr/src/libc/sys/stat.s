# @(#)stat.s	4.1 (Berkeley) 12/21/80
# C library -- stat

# error = stat(string, statbuf);

# char statbuf[36]

	.set	stat,18
.globl	_stat
.globl  cerror

_stat:
	.word	0x0000
	chmk	$stat
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
