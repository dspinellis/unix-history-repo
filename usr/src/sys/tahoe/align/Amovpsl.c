/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Amovpsl.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
movpsl(infop)	process_info *infop;
/*
/*	Move PSL to some place.
/*
/************************************/
{
	write_back (infop, psl, operand(infop,0));
}
