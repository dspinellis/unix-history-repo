#  C library -- ioctl
 
#  ioctl(fdes,command,arg)
#  struct * arg;
#
#  result == -1 if error
 
	.set	ioctl,54
.globl	_ioctl
.globl cerror
 
_ioctl:
	.word	0x0000
	chmk	$ioctl
	bcc	noerror
	jmp	cerror
noerror:
	ret
