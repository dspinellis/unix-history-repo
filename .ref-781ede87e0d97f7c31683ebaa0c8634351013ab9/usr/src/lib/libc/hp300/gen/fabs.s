/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)fabs.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

/* fabs - floating absolute value */

#include "DEFS.h"

ENTRY(fabs)
	fmoved	sp@(4),fp0
	fjnlt	L1
	fnegx	fp0
L1:
	fmoved	fp0,sp@-
	movel	sp@+,d0
	movel	sp@+,d1
	rts
