/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
	.asciz	"@(#)alloca.s	5.2 (Berkeley) %G%"
#endif not lint

/* like alloc, but automatic free in return */

#include "DEFS.h"

ENTRY(alloca, 0)
	subl2	4(ap),sp	/* crude allocation */
	movl	16(fp),r1	/* pc */
	movq	8(fp),ap	/* new (old) ap and fp */
	bicl2	$3,sp		/* 4-byte align */
	addl2	$7*4,sp		/* reuse space of mscp */
	movl	sp,r0		/* return value */
	jmp 	(r1)		/* funny return */
