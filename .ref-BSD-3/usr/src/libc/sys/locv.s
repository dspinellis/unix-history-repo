# C library -- long output conversion

.globl	_locv

_locv:
	.word	0x0000
	subl2	$8,sp
	cvtlp	4(ap),$11,(sp)
	editpc	$11,(sp),edpat,str
	skpc 	$' ,$11,str
	movl	r1,r0
	ret
	.data
edpat:	.byte	0xaa,0x01,0x91,0x44,0x00,0x00
str:	.space	13
