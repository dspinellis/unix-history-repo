/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: ntohl.s,v 1.1 92/06/25 12:47:06 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)ntohl.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* hostorder = ntohl(netorder) */

#include "DEFS.h"

ENTRY(ntohl)
	retl
	 nop
