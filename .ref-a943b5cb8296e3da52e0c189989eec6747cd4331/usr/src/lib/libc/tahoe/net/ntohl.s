#ifdef LIBC_SCCS
	.asciz	"@(#)ntohl.s	1.2 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* hostorder = ntohl(netorder) */

#include "DEFS.h"

ENTRY(ntohl, 0)
	movl	4(fp),r0
	ret
