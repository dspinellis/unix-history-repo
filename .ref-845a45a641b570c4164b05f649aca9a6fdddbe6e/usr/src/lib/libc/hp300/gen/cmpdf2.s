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
	.asciz "@(#)cmpdf2.s	8.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/* double > double: 1 */
/* double < double: -1 */
/* double == double: 0 */
ENTRY(__cmpdf2)
	fmoved	sp@(4),fp0
	fcmpd	sp@(12),fp0
	fbgt	Lagtb
	fslt	d0
	extbl	d0
	rts
Lagtb:
	moveq	#1,d0
	rts
