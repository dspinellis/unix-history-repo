/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Aput_byte.c	7.1 (Berkeley) %G%
 */

#include	"align.h"

put_byte (infop, byte, where)
process_info	*infop;
char		*where;
long		byte;
/*
/*	Put the byte at the given address in memory.
/*	Caveat: It's quite difficult to find a pte reference
/*		fault.  So I took the easy way out and just signal
/*		an illegal access.
/*	
/**************************************************/
{
	register long code;

	code = writeable(infop, where, 1);
	if ( code == TRUE ) {
		*where = byte;
	} else exception (infop, ILL_ACCESS, where, code);
}
