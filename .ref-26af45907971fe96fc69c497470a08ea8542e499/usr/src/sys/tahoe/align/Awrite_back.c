/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Awrite_back.c	7.1 (Berkeley) %G%
 */

#include "align.h"

write_back(infop,value, where)
process_info	*infop;
long		value; 
struct	oprnd 	*where;
/*
/*	Put the given result where the operand specifies. 
/*	
/*
/**************************************************/
{
	switch (where->length)
	{
		case 1: write_byte (infop,value, where); break;
		case 2: write_word (infop,value, where); break;
		case 4: write_longword (infop,value, where); break;
		case 8: write_quadword (infop,value, where); break;
		default : printf ("Wrong destination length in alignment\n");
	}
}
