/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)nargs.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* C library -- nargs */

#include "DEFS.h"

ENTRY(nargs, 0)
	movl	(fp),r0
	movw	-2(r0),r0  #  removed word
	subw2	$4,r0
	shar	$2,r0,r0  #  no. of arguments
	ret
