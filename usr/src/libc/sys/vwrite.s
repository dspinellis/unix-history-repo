# C library -- vwrite

# nwritten = vwrite(file, buffer, count);
#
# nwritten == -1 means error

	.set	vwrite,4+64
.globl	_vwrite
.globl  cerror

_vwrite:
	.word	0x0000
	chmk	$vwrite
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
