# @(#)chroot.s	4.1 (Berkeley) 12/21/80
#  C library -- chroot
 
#  error = chroot(string);
 
	.set	chroot,61
 
.globl	_chroot
.globl	cerror
_chroot:
	.word	0x0000
	chmk	$chroot
	bcc	noerror
	jmp	cerror
noerror:
	ret
