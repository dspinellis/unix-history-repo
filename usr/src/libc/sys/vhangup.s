# @(#)vhangup.s	4.1 (Berkeley) 12/21/80
# vhangup -- revoke access to terminal

	.set	vhangup,64+12
.globl	_vhangup

_vhangup:
	.word	0x0000
	chmk	$vhangup
	ret
