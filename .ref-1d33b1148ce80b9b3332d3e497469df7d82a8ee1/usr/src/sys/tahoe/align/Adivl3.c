/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Adivl3.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
divl3(infop) 	process_info *infop;
/*
/*	Arithmetic division, 3 operands.
/*
/**************************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register 	long	divident, divisor, result;

	divisor = operand(infop,0)->data;
	divident = operand(infop,1)->data;
	if (divisor == 0) {
		exception (infop, ARITHMETIC, 2);
	} else {
		Register_12=psl;
		Set_psl(r12);	/* restore the user psl */
		result = divident / divisor;
		asm ("	movpsl	r12");
		New_cc (Register_12);
	}
	write_back (infop,result, operand(infop,2));
}
