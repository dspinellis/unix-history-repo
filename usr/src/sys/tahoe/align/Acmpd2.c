/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Acmpd2.c	7.1 (Berkeley) %G%
 */

#include "align.h"
cmpd2(infop)	process_info *infop;
/*
/*	Compare operand 1 with operand 2 (double). 
/*
/****************************************************/
{
	register double	*Register_12;	/* Has to be first reg ! */
	register double	*Register_11;
	register long	Register_10;

	Register_12 = (double *) &operand(infop,0)->data;
	Register_11 = (double *) &operand(infop,1)->data;
	if ( reserved( *(long *)Register_12 ) ||
	     reserved( *(long *)Register_11 ) )
			exception(infop, ILL_OPRND);
	Register_10=psl;
	Set_psl(r10);	/* restore the user psl */
	asm ("	cmpd2	(r12),(r11)");
	asm ("	movpsl	r10");
	New_cc (Register_10);
}
