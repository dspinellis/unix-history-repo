#ifdef LIBC_SCCS
	.asciz	"@(#)subf.s	1.1 (Berkeley/CCI) 7/2/86"
#endif LIBC_SCCS

#include <tahoemath/fp.h>
#include "DEFS.h"

XENTRY(subf, R2|R3|R4|R5|R6|R7|R8|R9)
	clrl	r1
	pushl	12(fp)
	callf	$8,Xnegf
	pushl	r1
	pushl	r0
	pushl	8(fp)
	pushl	4(fp)
	callf	$20,Xaddf
	ret
