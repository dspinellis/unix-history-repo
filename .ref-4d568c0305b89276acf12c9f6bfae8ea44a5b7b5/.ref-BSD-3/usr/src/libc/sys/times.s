# C library -- times

	.set	times,43
.globl	_times

_times:
	.word	0x0000
	chmk	$times
	ret
