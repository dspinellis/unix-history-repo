/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Asubd.c	7.1 (Berkeley) %G%
 */

#include "align.h"
subd(infop)	process_info *infop;
/*
/*	Subtract operand from accumulator to accumulator (double).
/*
/*******************************************************************/
{
	register double 	*operand_pnt;
	register double		*acc_pnt;

	operand_pnt = (double *)&operand(infop,0)->data;
	acc_pnt = (double *) &acc_high;
	*acc_pnt = *acc_pnt - *operand_pnt;
}
