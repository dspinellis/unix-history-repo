/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Abicpsw.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
bicpsw(infop)	process_info *infop;
/*
/*	Bits clear in PSW.
/*
/*************************************/
{
	register int mask;

	mask = operand(infop,0)->data;
	psl &= ~(mask & 0x7f);
}
