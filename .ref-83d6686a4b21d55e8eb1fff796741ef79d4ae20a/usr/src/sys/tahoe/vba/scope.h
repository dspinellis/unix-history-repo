/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)scope.h	7.2 (Berkeley) %G%
 */

#ifdef DOSCOPE
/*  some i/o addresses used to generate pulses for scopes */
#define	OUT1	0xffffb034
#define	OUT2	0xffffb018
#define	OUT3	0xffffb020
#define	OUT4	0xffffb004
#define	OUT5	0xffffb024
#define	OUT6	0xffffb00c
#define	OUT7	0xffffb02c

#define	IOaddr(off)	(caddr_t)(&vmem[(off) & 0x0fffff])

extern	char vmem[];
extern	int cold;
#define	scope_out(x)	if (!cold) movob(IOaddr(OUT/**/x),0)
#define	scope_in(x)	if( !cold) dummy =  *IOaddr(IN/**/x)
#else
#define	scope_out(x)
#define	scope_in(x)
#endif
