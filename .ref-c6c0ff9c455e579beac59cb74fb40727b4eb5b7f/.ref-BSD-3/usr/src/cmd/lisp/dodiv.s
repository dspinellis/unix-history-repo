	.globl _dodiv
#
#	routine to destructively divide array representation of a bignum by 
#	1000000000
#
#	invocation:
#		remainder = dodiv(top,bottom)
#		int *top, *bottom;
#	where *bottom is the address of the biggning of the array, *top is
#	the top of the array.
#
#	register assignments:
#	r0 = carry
#	r1 & r2 = 64bit temporary
#	r3 = pointer
#
_dodiv:	.word	0
	clrl		r0		#no carry to begin.
	movl		8(ap),r3	#get pointer to array.
loop:	emul		$0x40000000,r0,(r3),r1
	ediv		$1000000000,r1,(r3),r0
	acbl		4(ap),$4,r3,loop
	ret
