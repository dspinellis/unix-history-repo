/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Asuba.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
suba(infop)	process_info *infop;
/*
/*	Subtract address.
/*
/****************************/
{
	register int data0, data1, result;

	data0 = operand(infop,0)->data;
	data1 = operand(infop,1)->data;
	result = data1 - data0;
	write_back (infop,result, operand(infop,1));
}
