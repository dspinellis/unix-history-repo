# @(#)acct.s	4.1 (Berkeley) 12/21/80
# C library -- acct

	.set	acct,51
.globl	_acct
.globl  cerror

_acct:
	.word	0x0000
	chmk	$acct
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
