/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Astd.c	7.1 (Berkeley) %G%
 */

#include "align.h"
std(infop)	process_info *infop;
/*
/*	Store accumulator (double) in destination.
/*
/*************************************************/
{
	quadword ac;

	ac.high = acc_high;
	ac.low = acc_low;
	write_quadword (infop, ac, operand(infop,0) );
	if (ac.high < 0) negative_1; else negative_0;
	if ( (ac.high & 0xff800000) == 0 ) zero_1; else zero_0;
	carry_1;
	overflow_0;
}
