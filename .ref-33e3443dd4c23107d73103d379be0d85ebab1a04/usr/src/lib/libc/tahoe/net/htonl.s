#ifdef LIBC_SCCS
	.asciz	"@(#)htonl.s	1.2 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* netorder = htonl(hostorder) */

#include "DEFS.h"

ENTRY(htonl, 0)
	movl	4(fp),r0
	ret
