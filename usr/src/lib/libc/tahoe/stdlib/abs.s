#ifdef LIBC_SCCS
	.asciz	"@(#)abs.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* abs - int absolute value */

#include "DEFS.h"

ENTRY(abs, 0)
	movl	4(fp),r0
	bgeq	1f
	mnegl	r0,r0
1:
	ret
