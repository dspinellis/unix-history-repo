/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)ffs.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* bit = ffs(value) */

#include "DEFS.h"

ENTRY(ffs)
	moveq	#-1,d0
	movl	sp@(4),d1
	beq	done
again:
	addql	#1,d0
	btst	d0,d1
	beq	again
done:
	addql	#1,d0
	rts
