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
 * from: $Header: saveregs.s,v 1.1 91/07/06 17:22:33 torek Exp $
 */

/*
 * Save register arguments in caller's `arg dump' area, so that
 * stdarg functions work.
 *
 * This really should be done with a pointer to the arg dump area;
 * our caller should allocate that area, not our caller's caller.
 * But then, they did not let me invent the calling sequence....
 *
 * We assume the caller has executed a `save' instruction.
 */
#include "DEFS.h"

ENTRY(__builtin_saveregs)
	st	%i0, [%fp + 0x44]	! fr->fr_argd[0]
	st	%i1, [%fp + 0x48]	! fr->fr_argd[1]
	st	%i2, [%fp + 0x4c]	! fr->fr_argd[2]
	st	%i3, [%fp + 0x50]	! fr->fr_argd[3]
	st	%i4, [%fp + 0x54]	! fr->fr_argd[4]
	retl
	st	%i5, [%fp + 0x58]	! fr->fr_argd[5]
