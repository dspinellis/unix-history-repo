# @(#)read.s	4.1 (Berkeley) 12/21/80
# C library -- read

# nread = read(file, buffer, count);
#
# nread ==0 means eof; nread == -1 means error

	.set	read,3
.globl	_read
.globl  cerror

_read:
	.word	0x0000
	chmk	$read
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
