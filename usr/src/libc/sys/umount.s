# @(#)umount.s	4.1 (Berkeley) 12/21/80
# C library -- umount/

	.set	umount,22
.globl	_umount
.globl	cerror
.comm	_errno,4

_umount:
	.word	0x0000
	chmk	$umount
	bcc 	noerror
	jmp 	cerror
noerror:
	clrl	r0
	ret
