# C library -- fstat

# error = fstat(file, statbuf);

# char statbuf[34]

	.set	fstat,28
.globl	_fstat

_fstat:
	.word	0x0000
	chmk	$fstat
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
