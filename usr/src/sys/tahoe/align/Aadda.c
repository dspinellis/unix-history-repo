/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Aadda.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
adda(infop)	process_info *infop;
/*
/*	Add address.
/*
/************************/
{
	register long result;

	result = operand(infop,1)->data + operand(infop,0)->data;
	write_back (infop,result, operand(infop,1));
}
