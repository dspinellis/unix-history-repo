# @(#)close.s	4.1 (Berkeley) 12/21/80
# C library -- close

# error =  close(file);

	.set	close,6
.globl	_close

_close:
	.word	0x0000
	chmk	$close
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
