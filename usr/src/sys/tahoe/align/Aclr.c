/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Aclr.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
clr(infop)	process_info *infop;
/*
/*	Clear operand
/*
/*************************************/
{

	write_back(infop, 0, operand(infop,0));
	negative_0;
	zero_1;
	overflow_0;
	carry_1;
}
