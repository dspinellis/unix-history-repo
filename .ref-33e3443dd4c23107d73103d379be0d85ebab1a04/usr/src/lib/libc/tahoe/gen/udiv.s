#ifdef LIBC_SCCS
	.asciz	"@(#)udiv.s	1.1 (Berkeley/CCI) %G%"
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
	clrl	r0			#  return(dividend/divisor);
	movl	4(fp),r1
	ediv	8(fp),r0,r2,r3
	movl	r2,r0
	ret
