/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#include "SYS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR "@(#)_exit.s	5.7 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

	.align	1
PSEUDO(_exit,exit)	/* _exit(status) */
