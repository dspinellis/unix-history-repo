/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include <machine/machAsmDefs.h>

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)htons.s	8.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

/*
 * netorder = htons(hostorder)
 * hostorder = ntohs(netorder)
 */
NLEAF(htons)
ALEAF(ntohs)
#ifdef MIPSEL
	srl	v0, a0, 8
	and	v0, v0, 0xff
	sll	v1, a0, 8
	and	v1, v1, 0xff00
	or	v0, v0, v1
#else
#ifdef MIPSEB
	move	v0, a0
#else
	ERROR
#endif
#endif
	j	ra
END(htons)
