#ifdef LIBC_SCCS
	.asciz	"@(#)strlen.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/*
 * Returns the number of
 * non-NULL bytes in string argument.
 */

#include "DEFS.h"

ENTRY(strlen, 0)
	movl	4(fp),r0
	movl	r0,r1
	cmps2
	subl2	4(fp),r0
	ret
