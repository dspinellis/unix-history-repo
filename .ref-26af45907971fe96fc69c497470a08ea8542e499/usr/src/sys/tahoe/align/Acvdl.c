/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Acvdl.c	7.1 (Berkeley) %G%
 */

#include "align.h"
cvdl(infop)	process_info *infop;
/*
/*	Convert double precission accumulator into integer.
/*
/******************************************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	double	*acc_pnt;
	register	long	result;

	acc_pnt = (double *)&acc_high;
	Register_12=psl;
	Set_psl(r12);	/* restore the user psl */
	result = (long) *acc_pnt;
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop, result, operand(infop,0) );
}
