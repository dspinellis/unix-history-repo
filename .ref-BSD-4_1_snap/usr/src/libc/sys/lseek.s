# @(#)lseek.s	4.1 (Berkeley) 12/21/80
# C library -- lseek

# error = lseek(file, offset, ptr);

	.set	lseek,19
.globl	_lseek
.globl  cerror

_lseek:
	.word	0x0000
	chmk	$lseek
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
