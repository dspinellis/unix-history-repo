#ifdef LIBC_SCCS
	.asciz	"@(#)strncpy.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/*
 * Copy s2 to s1, truncating or null-padding to always copy n bytes
 * return s1
 */
#include "DEFS.h"

ENTRY(strncpy, 0)
	movl	4(fp),r1
	movl	8(fp),r0
	movl	12(fp),r2
	movl	r1,r3
	movs3
	movl	r3,r0
	ret
