/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 * from: $Header: syscall.s,v 1.1 91/07/06 13:06:02 torek Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)syscall.s	5.2 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(syscall)
	mov	0|SYSCALL_G2RFLAG, %g1	! 0 == indir
	add	%o7, 8, %g2
	t	ST_SYSCALL
	ERROR()
