#ifdef LIBC_SCCS
	.asciz	"@(#)ffs.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* bit = ffs(value) */

#include "DEFS.h"

ENTRY(ffs, 0)
	ffs	4(fp),r0
	bgeq	1f
	mnegl	$1,r0
1:
	incl	r0
	ret
