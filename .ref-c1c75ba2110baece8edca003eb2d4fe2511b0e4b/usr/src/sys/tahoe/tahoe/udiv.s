/*
 *	@(#)udiv.s	7.1 (Berkeley) %G%
 */

#include "../tahoe/SYS.h"

/*
 * result = udiv(dividend, divisor)
 *
 * unsigned long division
 */
	.text
ASENTRY(udiv, R2|R3)
	bitl	$0x80000000,8(fp)	# if (divisor & 0x80000000) {
	jeql	2f			  
	cmpl	8(fp),4(fp)		# if (divisor > dividend)
	jlequ	1f
	clrl	r0			#	return (0);
	ret
1:					# else
	movl	$1,r0			#  	return (1);
	ret				# }
2:
	clrl	r0			# return (dividend / divisor);
	movl	4(fp),r1
	ediv	8(fp),r0,r2,r3
	movl	r2,r0
	ret
