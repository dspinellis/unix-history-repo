#  C library -- umask
 
#  omask = umask(mode);
 
	.set	umask,60
.globl	_umask
.globl	cerror

_umask:
	.word	0x0000
	chmk	$umask
	bcc	noerror
	jmp	cerror
noerror:
	ret
