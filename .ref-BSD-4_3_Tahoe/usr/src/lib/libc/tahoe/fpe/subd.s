#ifdef LIBC_SCCS
	.asciz	"@(#)subd.s	1.1 (Berkeley/CCI) 7/2/86"
#endif LIBC_SCCS

#include <tahoemath/fp.h>
#include "DEFS.h"

XENTRY(subd, 0)
	pushl	16(fp)
	pushl	12(fp)
	callf	$12,Xnegd
	pushl	r1
	pushl	r0
	pushl	8(fp)
	pushl	4(fp)
	callf	$20,Xaddd
	ret
