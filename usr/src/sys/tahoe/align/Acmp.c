/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Acmp.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
cmp(infop)	process_info *infop;
/*
/*	Arithmetic comparison 
/*
/**************************************************/
{
	register long	Register_12;	/* Has to be first reg ! */
	register long	Register_11;
	register long	Register_10;

	Register_12 = operand(infop,0)->data;
	Register_11 = operand(infop,1)->data;
	Register_10=psl;
	Set_psl(r10);	/* restore the user psl */
	asm ("	cmpl	r12,r11");
	asm ("	movpsl	r12");
	New_cc (Register_12);
}
