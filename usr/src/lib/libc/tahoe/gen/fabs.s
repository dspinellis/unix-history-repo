#ifdef LIBC_SCCS
	.asciz	"@(#)fabs.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

#include "DEFS.h"

ENTRY(fabs, 0)
	movl	8(fp),r1
	movl	4(fp),r0
	bgeq	1f
	xorl2	$0x80000000,r0
1:
	ret
