/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Alnd.c	7.1 (Berkeley) %G%
 */

#include "align.h"
lnd(infop)	process_info *infop;
/*
/*	Load a negated double operand into accumulator.
/*
/*******************************************************/
{
	register struct oprnd *oprnd_pnt;

	oprnd_pnt = operand(infop,0);
	if ( reserved( oprnd_pnt->data ) ) 
		exception(infop, ILL_OPRND);
	if ( oprnd_pnt->data == 0 ) acc_high = 0;
	else acc_high = 0x80000000 ^ oprnd_pnt->data ;
	acc_low = oprnd_pnt->data2 ;
	psl |= PSL_DBL;
	infop->acc_dbl = 1;
}
