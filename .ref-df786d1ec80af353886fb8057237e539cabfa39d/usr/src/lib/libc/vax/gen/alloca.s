/*	alloca.s	4.1	83/06/27	*/
/* like alloc, but automatic automatic free in return */

#include "DEFS.h"

ENTRY(alloca)
	subl2	4(ap),sp	/* crude allocation */
	movl	16(fp),r1	/* pc */
	movq	8(fp),ap	/* new (old) ap and fp */
	bicl2	$3,sp		/* 4-byte align */
	addl2	$7*4,sp		/* reuse space of mscp */
	movl	sp,r0		/* return value */
	jmp 	(r1)		/* funny return */
