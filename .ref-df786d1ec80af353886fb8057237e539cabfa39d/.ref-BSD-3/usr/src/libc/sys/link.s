# C library -- link

# error = link(old-file, new-file);
#

	.set	link,9
.globl	_link
.globl cerror

_link:
	.word	0x0000
	chmk	$link
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
