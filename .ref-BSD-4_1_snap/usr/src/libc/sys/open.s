# @(#)open.s	4.1 (Berkeley) 12/21/80
# C library -- open

# file = open(string, mode)
#
# file == -1 means error

	.set	open,5
.globl	_open
.globl  cerror

_open:
	.word	0x0000
	chmk	$open
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
