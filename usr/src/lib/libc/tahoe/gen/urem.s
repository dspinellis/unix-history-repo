#ifdef LIBC_SCCS
	.asciz	"@(#)urem.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/*
 * Unsigned remainder.
 *
 * urem(dividend, divisor)
 */
#include "DEFS.h"

ASENTRY(urem, 0)
	bitl	$0x80000000,8(fp)	#  if (divisor & 0x80000000){
	jeql	1f			  
	movl	4(fp),r0
	cmpl	8(fp),r0		#  if (divisor <= dividend )
	jgtru	2f
	subl2	8(fp),r0		#       return(dividend-divisor);
2:					#  else return(dividend);}
	ret
1:
	clrl	r2			#  return(dividend%divisor);
	movl	4(fp),r3
	ediv	8(fp),r2,r1,r0
	ret

/*
 * aurem(dividendp, divisor) -- like urem but uses address of dividend.
 *	Implements %= avoiding side effects in the dividend expression.
 */
ASENTRY(aurem, 0)
	bitl	$0x80000000,8(fp)	#  if (divisor & 0x80000000){
	jeql	1f			  
	movl	*4(fp),r0
	cmpl	8(fp),r0		#  if (divisor <= dividend )
	jgtru	2f
	subl2	8(fp),r0		#       return(dividend-divisor);
					#  else return(dividend);}
	jbr	2f
1:
	clrl	r2			#  return(dividend%divisor);
	movl	*4(fp),r3
	ediv	8(fp),r2,r1,r0
2:
	movl	r0,*4(fp)
	ret
