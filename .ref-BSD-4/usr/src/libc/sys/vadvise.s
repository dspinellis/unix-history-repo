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
