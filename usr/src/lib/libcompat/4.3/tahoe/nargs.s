#ifdef LIBC_SCCS
	.asciz	"@(#)nargs.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* C library -- nargs */

#include "DEFS.h"

ENTRY(nargs, 0)
	movw	-2(fp),r0  #  removed word
	subw2	$4,r0
	shar	$2,r0,r0  #  no. of arguments
	ret
