# C library -- mount

# error = mount(dev, file, flag)

	.set	mount,21
.globl	_mount
.globl  cerror

_mount:
	.word	0x0000
	chmk	$mount
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
