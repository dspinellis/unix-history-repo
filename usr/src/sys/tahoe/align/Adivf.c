/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Adivf.c	7.1 (Berkeley) %G%
 */

#include "align.h"
divf(infop)	process_info *infop;
/*
/*	Divide accumulator by operand to accumulator (float).
/*
/*******************************************************************/
{
	register float	*operand_pnt;
	register float	*acc_pnt;

	operand_pnt = (float *)&operand(infop,0)->data;
	acc_pnt = (float *) &acc_high;
	*acc_pnt = *acc_pnt / *operand_pnt;
}
