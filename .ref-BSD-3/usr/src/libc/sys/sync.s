	.set	sync,36
.globl	_sync

_sync:
	.word	0x0000
	chmk	$sync
	ret
