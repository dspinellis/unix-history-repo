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
