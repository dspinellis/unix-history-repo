/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Apusha.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
pusha(infop)	process_info *infop;
/*
/*	Push address of the operand
/*
/**************************************/
{
	register long new_address;

	new_address = operand(infop,0)->address;
	if (new_address < 0) negative_1; else negative_0;
	if (new_address == 0) zero_1; else zero_0;
	overflow_0; carry_1;
	push (infop, new_address);
}
