	.globl	_gstart
	.globl	_proflush
	.globl	_holbeg
	.globl	_holend
_gstart:
	.word	0
	moval	start,r0
	ret
_proflush:
	.word	0
	ret
#
	.data
_holbeg:			# dummy locations
_holend:
