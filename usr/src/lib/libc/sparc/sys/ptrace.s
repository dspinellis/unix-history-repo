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
 * from: $Header: ptrace.s,v 1.2 91/12/20 01:59:00 leres Exp $
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)ptrace.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "SYS.h"

ENTRY(ptrace)
	sethi	%hi(_errno), %g1
	st	%g0, [%g1 + %lo(_errno)]
	mov	SYS_ptrace, %g1
	t	ST_SYSCALL
	bcc	1f
	nop
	ERROR()
1:
	retl
	nop
