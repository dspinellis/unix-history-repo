/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Atst.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
tst(infop)	process_info *infop;
/*
/*	Test operand, set condition codes.
/*
/************************************************/
{
	register long quantity;

	quantity = operand(infop,0)->data;
	if (quantity < 0) negative_1; else negative_0;
	if (quantity == 0) zero_1; else zero_0;
	overflow_0; carry_1;
}
