/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)Awrite_byte.c	7.1 (Berkeley) 12/6/90
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
