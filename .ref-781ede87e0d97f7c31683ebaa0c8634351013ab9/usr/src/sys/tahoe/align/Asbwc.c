/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Asbwc.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
sbwc(infop)	process_info *infop;
/*
/*	Subtract with carry.
/*	Note : the play with 'tmp' just before the 'asm' line makes
/*		sure that when the sbwc opcode is executed, the current
/*		carry in psl is the same as the 'offending' process'.
/*		Don't change unless you know exactly what you're doing.
/*
/*****************************/
{
	register	long	Register_12;	/* Has to be first reg ! */
	register	long	Register_11;
	register	long	Register_10;
	register 	long	tmp;

	Register_12 = operand(infop,0)->data;
	Register_11 = operand(infop,1)->data;
	if (carry)	/* If process' carry set */
		tmp = -1;
	else tmp = 0;
	tmp++;		/* 0 => carry set.  1 => carry clear */

	Register_10=psl;
	Set_psl(r10);	/* restore the user psl */
	asm("	sbwc	r12,r11");
	asm("	movpsl	r12");
	New_cc (Register_12);

	write_back(infop, Register_11, operand(infop,1) );
}
