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
	.asciz "@(#)cmpsf2.s	5.1 (Berkeley) %G%"
#endif /* LIBC_SCCS and not lint */

#include "DEFS.h"

/* single > single: 1 */
/* single < single: -1 */
/* single == single: 0 */
ENTRY(__cmpsf2)
	fmoves	sp@(4),fp0
	fcmps	sp@(8),fp0
	fbgt	Lagtb
	fslt	d0
	extbl	d0
	rts
Lagtb:
	moveq	#1,d0
	rts
