# C library - getcsw

# csw = getcsw();

	.set	getcsw,38
.globl	_getcsw

_getcsw:
	.word	0x0000
	chmk	$getcsw
	ret
