#ifdef LIBC_SCCS
	.asciz	"@(#)insque.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* insque(new, pred) */

#include "DEFS.h"

ENTRY(insque, 0)
	insque	*4(fp), *8(fp)
	ret
