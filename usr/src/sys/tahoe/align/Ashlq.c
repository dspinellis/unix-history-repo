/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Ashlq.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
shlq(infop) 	process_info *infop;
/*
/*	Shift left quadword.
/*
/********************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register 	long	Register_11;
	register 	long	Register_10;
	register	long	*Register_9;
	quadword	result;

	Register_12 = operand(infop,0)->data;	/* Counter */
	Register_9 = &operand(infop,1)->data;	/* Source */

	Register_10=psl;
	Set_psl(r10);	/* restore the user psl */
	asm ("	shlq	r12,(r9),r10");
	asm ("	movpsl	r12");
	New_cc (Register_12);
	result.high = Register_10;
	result.low  = Register_11;
	write_quadword (infop, result, operand(infop,2));
}
