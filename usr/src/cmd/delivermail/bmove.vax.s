#
#  BMOVE.S -- optimized block move routine.
#
#	@(#)bmove.vax.s	2.1	11/5/80
#
.globl	_bmove
_bmove:
	.word	0x0030
	movc3	12(ap),*4(ap),*8(ap)
	ret
