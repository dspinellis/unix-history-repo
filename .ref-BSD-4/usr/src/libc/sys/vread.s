# C library -- vread

# nread = vread(file, buffer, count);
#
# nread ==0 means eof; nread == -1 means error

	.set	vread,3+64
.globl	_vread
.globl  cerror

_vread:
	.word	0x0000
	chmk	$vread
	bcc 	noerror
	jmp 	cerror
noerror:
	ret
