/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Aput_long.c	7.1 (Berkeley) %G%
 */

#include	"align.h"

put_longword (infop, longword, where)
register	process_info	*infop;
register	char		*where;
register	long		longword;
/*
/*	Put the longword at the given address in memory.
/*	Caveat: It's quite difficult to find a pte reference
/*		fault.  So I took the easy way out and just signal
/*		an illegal access.
/*	
/**************************************************/
{
	register long code;

	code = writeable(infop, where, 4);
	if ( code == TRUE ) {
		*where++ = longword>>24;
		*where++ = longword>>16;
		*where++ = longword>>8;
		*where = longword;
	} else exception (infop, ILL_ACCESS, where, code);
}
