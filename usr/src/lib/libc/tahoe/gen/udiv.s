#ifdef LIBC_SCCS
	.asciz	"@(#)udiv.s	1.2 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/*
 * Unsigned divide.
 *
 * udiv(dividend, divisor)
 */
#include "DEFS.h"

ASENTRY(udiv, 0)
	bitl	$0x80000000,8(fp)	#  if (divisor & 0x80000000){
	jeql	1f			  
	cmpl	8(fp),4(fp)		#  if (divisor > dividend )
	jlequ	2f
	clrl	r0			#      return(0);
	ret
2:					#  else
	movl	$1,r0			#      return(1);}
	ret
1:
	clrl	r2			#  return(dividend/divisor);
	movl	4(fp),r3
	ediv	8(fp),r2,r0,r1
	ret

/*
 * audiv(dividendp, divisor) -- like udiv but uses address of dividend.
 *	Implements /= avoiding side effects in the dividend expression.
 */
ASENTRY(audiv, 0)
	bitl	$0x80000000,8(fp)	#  if (divisor & 0x80000000){
	jeql	1f			  
	cmpl	8(fp),*4(fp)		#  if (divisor > dividend )
	jlequ	2f
	clrl	r0			#      return(0);
	jbr	3f
2:					#  else
	movl	$1,r0			#      return(1);}
	jbr	3f
1:
	clrl	r2			#  return(dividend/divisor);
	movl	*4(fp),r3
	ediv	8(fp),r2,r0,r1
3:
	movl	r0,*4(fp)
	ret
