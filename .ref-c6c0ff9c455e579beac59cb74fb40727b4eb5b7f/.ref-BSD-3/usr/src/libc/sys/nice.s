# C library-- nice

# error = nice(hownice)

	.set	nice,34
.globl	_nice
.globl  cerror

_nice:
	.word	0x0000
	chmk	$nice
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
