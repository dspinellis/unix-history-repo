#ifdef LIBC_SCCS
	.asciz	"@(#)strcpy.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/*
 * Copy string s2 to s1.  s1 must be large enough.
 * return s1
 */
#include "DEFS.h"

ENTRY(strcpy, 0)
	movl	4(fp),r1
	movl	r1,r2
	movl	8(fp),r0
	movs2
	movl	r2,r0
	ret
