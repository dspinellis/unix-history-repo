/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Astf.c	7.1 (Berkeley) %G%
 */

#include "align.h"
stf(infop)	process_info *infop;
/*
/*	Store accumulator (float) in destination.
/*
/*************************************************/
{

	write_back (infop, acc_high, operand(infop,0) );
	if (acc_high < 0) negative_1; else negative_0;
	if ( (acc_high & 0xff800000) == 0 ) zero_1; else zero_0;
	carry_1; overflow_0;
}
