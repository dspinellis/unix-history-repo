/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Abbc.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
bbc(infop)	process_info *infop;
/*
/*	Branch on bit clear.
/*
/********************************/
{
	register int position,base, new_address;

	position = operand(infop,0)-> data & 0x1f;
	base = operand(infop,1)->data;
	new_address = operand(infop,2) -> address;
	negative_0; zero_1; overflow_0; carry_1;
	if ( !(base & 1 << position) ) pc = new_address;
}
