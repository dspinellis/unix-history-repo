/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)udivsi3.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/*
 * Unsigned divide for GCC.
 *
 * __udivsi3(dividend, divisor)
 */
#include "DEFS.h"

ENTRY(__udivsi3, 0)
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
