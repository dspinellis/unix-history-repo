/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#include "SYS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR "@(#)_getlogin.s	5.4 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* internal version of getlogin() */

PSEUDO(_getlogin,getlogin)	/* _getlogin(buf, buflen) */
	ret
