/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	ASMSTR("@(#)htonl.s	5.1 (Berkeley) %G%")
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/*
 * netorder = htonl(hostorder)
 * hostorder = ntohl(netorder)
 */
LEAF(htonl)				# a0 = 0x11223344, return 0x44332211
ALEAF(ntohl)
	srl	v1, a0, 24		# v1 = 0x00000011
	sll	v0, a0, 24		# v0 = 0x44000000
	or	v0, v0, v1
	and	v1, a0, 0xff00
	sll	v1, v1, 8		# v1 = 0x00330000
	or	v0, v0, v1
	srl	v1, a0, 8
	and	v1, v1, 0xff00		# v1 = 0x00002200
	or	v0, v0, v1
	j	ra
END(htonl)
