/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Ajmp.c	7.1 (Berkeley) %G%
 */

#include "align.h" 
jmp(infop)
process_info *infop;
/*
/*	Jump to the given address.
/*
/********************************************/
{
	pc = operand(infop,0)->address ;
}
