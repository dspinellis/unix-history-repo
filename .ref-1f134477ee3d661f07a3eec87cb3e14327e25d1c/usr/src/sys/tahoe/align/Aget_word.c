/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Aget_word.c	7.1 (Berkeley) %G%
 */

#include	"align.h"
int get_word (infop, address)
process_info	*infop;
char		*address;
/*
/*	Fetch the word at the given 'address' from memory.
/*	Caveat: It's quite difficult to find a pte reference
/*		fault.  So I took the easy way out and just signal
/*		an illegal access.
/*	
/**************************************************/
{
	register long code, value;

	code = readable(infop, address, 2);
	if (code == TRUE) {
		value = *address++ << 8;
		value = value | *address & 0xff;
		return(value);
	} else exception (infop, ILL_ACCESS, address, code);
}
