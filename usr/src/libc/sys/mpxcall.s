# @(#)mpxcall.s	4.1 (Berkeley) 12/21/80
# C library -- mpxcall

# mpxcall(file, buffer, count);
#
#

	.set	mpxcall,56
.globl	_mpxcall
.globl  cerror

_mpxcall:
	.word	0x0000
	chmk	$mpxcall
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
