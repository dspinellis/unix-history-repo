#ifdef LIBC_SCCS
	.asciz	"@(#)sfp_exp.s	1.1 (Berkeley/CCI) 7/2/86"
#endif LIBC_SCCS

#include <tahoemath/fp.h>
#include "DEFS.h"

/*
 * Reserved floating point operand.
 */
ASENTRY(sfpresop, 0)
	movl	$0xaaaaaaaa,r0
	clrl	r1
	ret

/*
 * Floating point overflow.
 */
ASENTRY(sfpover, 0)
	movl	$HUGE0,r0
	clrl	r1
	ret

/*
 * Floating point underflow.
 */
ASENTRY(sfpunder, 0)
	clrl	r0
	clrl	r1
	ret

/*
 * Floating point division by zero.
 */
ASENTRY(sfpzdiv, 0)
	divl2	$0,r0		# force division by zero.
	ret
