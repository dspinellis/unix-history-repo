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
 * from: $Header: reboot.s,v 1.1 91/07/06 13:05:59 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)reboot.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

SYSCALL(reboot)
	unimp	0
