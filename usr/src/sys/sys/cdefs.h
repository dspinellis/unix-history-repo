/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)cdefs.h	7.1 (Berkeley) %G%
 */

/*
 * This file is designed to ease the porting from standard C to ANSI C.
 * It will eventually go away.
 * Questions to K. Bostic.
 */

#ifdef __STDC__
#define	CONCAT(x,y)	x ## y
#define	PROTOTYPE(p)	p
#define	STRING(x)	#x
#else
#define	const
#define	volatile
#define	signed
#define	CONCAT(x,y)	x/**/y
#define	PROTOTYPE(p)	()
#define	STRING(x)	"x"
#endif
