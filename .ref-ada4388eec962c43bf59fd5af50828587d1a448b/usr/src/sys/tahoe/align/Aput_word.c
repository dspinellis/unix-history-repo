/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Aput_word.c	7.1 (Berkeley) %G%
 */

#include	"align.h"

put_word (infop, word, where)
register	process_info	*infop;
register	char		*where;
register	long		word;
/*
/*	Put the word at the given address in memory.
/*	Caveat: It's quite difficult to find a pte reference
/*		fault.  So I took the easy way out and just signal
/*		an illegal access.
/*	
/**************************************************/
{
	register long code;

	code = writeable(infop, where, 2);
	if ( code == TRUE ) {
		*where = word>>8;
		*(where+1) = word;
	} else exception (infop, ILL_ACCESS, where, code);
}
