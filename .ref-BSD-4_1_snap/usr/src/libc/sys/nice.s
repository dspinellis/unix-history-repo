# @(#)nice.s	4.1 (Berkeley) 12/21/80
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
