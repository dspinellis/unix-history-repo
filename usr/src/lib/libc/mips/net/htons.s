/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#include "DEFS.h"

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)htons.s	5.3 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

#include <machine/endian.h>
#undef	htons
#undef	ntohs

/*
 * netorder = htons(hostorder)
 * hostorder = ntohs(netorder)
 */
LEAF(htons)
ALEAF(ntohs)
#if BYTE_ORDER == LITTLE_ENDIAN
	srl	v0, a0, 8
	and	v0, v0, 0xff
	sll	v1, a0, 8
	and	v1, v1, 0xff00
	or	v0, v0, v1
#else
#if BYTE_ORDER == BIG_ENDIAN
	move	v0, a0
#else
	ERROR
#endif
#endif
	j	ra
END(htons)
