/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Knegd.s	7.1 (Berkeley) %G%
 */

#include "../math/fp.h"
#include "../math/Kfp.h"
#include "../tahoe/SYS.h"

	.text
ENTRY(Knegd, 0)
	andl3	$EXPMASK,4(fp),r0	/* check for reserved operand,zero. */
	beql	retzero
	movl	4(fp),r0		/* fetch operand. */
	movl	8(fp),r1
	bbc	$31,r0,seton
	andl2	$(0!SIGNBIT),r0		/* turn it off. */
	ret
seton:	orl2	$SIGNBIT,r0		/* turn it on. */
	ret
retzero:
	clrl	r0
	clrl	r1
	ret
