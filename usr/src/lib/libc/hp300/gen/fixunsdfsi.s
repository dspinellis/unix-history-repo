/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fixunsdfsi.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/* (unsigned) double */
ENTRY(__fixunsdfsi)
	fintrzd	sp@(4),fp0
	fcmpd	#0r2147483648.0,fp0
	fbge	Lwaybig
	fmovel	fp0,d0
	rts
Lwaybig:
	fsubd	#0r2147483648.0,fp0
	fmovel	fp0,d0
	bset	#31,d0
	rts
