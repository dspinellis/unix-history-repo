#
#  BMOVE.S -- optimized block move routine.
#
#	@(#)bmove.vax.s	3.1	3/7/81
#
.globl	_bmove
_bmove:
	.word	0x0030
	movc3	12(ap),*4(ap),*8(ap)
	ret
