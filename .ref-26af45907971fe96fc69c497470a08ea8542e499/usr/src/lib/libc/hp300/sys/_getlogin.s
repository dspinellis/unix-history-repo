/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)_getlogin.s	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

PSEUDO(_getlogin,getlogin)	/* _getlogin(buf, buflen) */
	ret
