/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Amov.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
mov(infop) 	process_info *infop;
/*
/*	Move operand
/*
/****************************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	long	Register_11;

	Register_12 = operand(infop, 0)->data;
	Register_11=psl;
	Set_psl(r11);	/* restore the user psl */
	asm ("	movl	r12,r11");
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop, Register_11, operand(infop,1));
}	
