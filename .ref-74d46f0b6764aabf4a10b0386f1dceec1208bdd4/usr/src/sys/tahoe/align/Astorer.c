/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Astorer.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
storer(infop)	process_info *infop;
/*
/*	Store multiple registers.
/*
/***************************************/
{
	register int mask, next_register, new_address;

	mask = operand(infop,0)->data & 0x3fff;	/* Bits 0 to 13 only */
	new_address = operand(infop,1)->address;
	next_register = 0;				/* Register # */
	while (next_register <= 13)
	{ 
		if (mask & 1 << next_register) 
		{
			put_longword (infop, Register (infop, next_register),
				new_address);
			new_address += 4; 
		}
		next_register++;
	}
}
