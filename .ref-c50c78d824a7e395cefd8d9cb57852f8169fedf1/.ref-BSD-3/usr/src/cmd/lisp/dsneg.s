	.globl	_dsneg
#
#	dsneg(top, bot);
#	int *top, *bot;
#
#	routine to destructively negate a bignum stored in array format
#	lower order stuff at higher addresses. It is assume that the
#	result will be positive.
#	
_dsneg:	.word	0
	movl	4(ap),r1	#load up address.
	clrl	r2		#set carry
loop:	mnegl	(r1),r0		#negate and take carry into account.
	addl2	r2,r0
	extzv	$0,$30,r0,(r1)
	extv	$30,$2,r0,r2
	acbl	8(ap),$-4,r1,loop
				#decrease r1, and branch back if appropriate.
	ret
