# @(#)vadvise.s	4.1 (Berkeley) 12/21/80
# C library -- vadvise

# error =  vadvise(how);

	.set	vadvise,64+8
.globl	_vadvise

_vadvise:
	.word	0x0000
	chmk	$vadvise
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
