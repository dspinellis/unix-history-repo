/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)alloca.s	5.5 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

ENTRY(alloca, 0)
	movl	4(ap),r0	# get allocation size
	movl	16(fp),r2	# save return address before we smash it
	movab	here,16(fp)
	ret
here:
	subl2	r0,sp		# create stack space
	bicl2	$3,sp		# align to longword boundary
	movl	sp,r0
	jmp	(r2)
