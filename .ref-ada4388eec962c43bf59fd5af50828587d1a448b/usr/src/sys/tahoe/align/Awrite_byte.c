/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Awrite_byte.c	7.1 (Berkeley) %G%
 */

#include	"align.h"

write_byte (infop,byte, where)
process_info	*infop;
long		byte; 
struct oprnd 	*where;
/*
/*	Put the 'byte' at the given address in
/*	tahoe's memory.
/*	
/*	1. Only the least significant byte is written.
/*
/**************************************************/
{
	register struct operand_des *look_at;

	look_at = &Table[opCODE].operand[last_operand];
	if (! (look_at->add_modes & NOVF))
		if (byte > 0x7f || byte < -0x80) overflow_1;	
	if (!(where->mode & W)) exception(infop, ILL_ADDRMOD);
	switch (where->mode & ADDFIELD)	/* Mask out R/W bits */
	{
	case Add:
		put_byte(infop, byte, where->address);
		break;
	case Dir:
		Replace (infop, where->reg_number, byte);
		break;
	case SPmode:
		where->mode = where->mode & ~SPmode | Add;
		write_longword (infop, byte, where);
		break;
	default:
		printf("Unknown destination in write_byte (alignment code)\n");
	};
}	
