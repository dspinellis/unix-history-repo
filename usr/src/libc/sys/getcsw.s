# @(#)getcsw.s	4.1 (Berkeley) 12/21/80
# C library - getcsw

# csw = getcsw();

	.set	getcsw,38
.globl	_getcsw

_getcsw:
	.word	0x0000
	chmk	$getcsw
	ret
