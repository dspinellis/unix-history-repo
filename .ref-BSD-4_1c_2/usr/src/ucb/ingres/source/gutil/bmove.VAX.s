#
#  BMOVE.S -- optimized block move routine.
#
#	@(#)bmove.VAX.s	7.1	2/5/81
#
.globl	_bmove
_bmove:
	.word	0x0030
	movc3	12(ap),*4(ap),*8(ap)
	ret
