/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Acvtlw.c	7.1 (Berkeley) %G%
 */

#include "align.h"
cvtlw(infop) 
process_info *infop;
/*
/*	Convert longword to word
/*
/****************************************/
{
	register long result;

	result = operand(infop,0)->data;
	if (result < 0 )  negative_1 ; else negative_0;
	if (result == 0 )  zero_1 ; else zero_0;
	carry_1; 
	if (result > 0x7fff || result <= -0x8000) overflow_1;
	else overflow_0;
	write_back (infop, result, operand(infop,1) );
}
