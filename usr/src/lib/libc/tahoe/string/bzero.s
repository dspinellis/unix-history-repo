#ifdef LIBC_SCCS
	.asciz	"@(#)bzero.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* bzero(base, length) */
#include "DEFS.h"

ENTRY(bzero, 0)
	movl	$1f,r0				# r0 = null source string
	movl	4(fp),r1			# r1 = destination address
	movl	8(fp),r2			# r2 = count
	movs3
	ret
1:
	.byte 0
