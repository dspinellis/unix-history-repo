# @(#)write.s	4.1 (Berkeley) 12/21/80
# C library -- write

# nwritten = write(file, buffer, count);
#
# nwritten == -1 means error

	.set	write,4
.globl	_write
.globl  cerror

_write:
	.word	0x0000
	chmk	$write
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
