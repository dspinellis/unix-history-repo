#ifdef LIBC_SCCS
	.asciz	"@(#)ntohs.s	1.2 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* hostorder = ntohs(netorder) */

#include "DEFS.h"

ENTRY(ntohs, 0)
	movzwl	6(fp),r0
	ret
