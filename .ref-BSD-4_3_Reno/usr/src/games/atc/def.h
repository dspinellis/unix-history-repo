/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ed James.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)def.h	5.2 (Berkeley) 4/30/90
 */

/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

#define AUTHOR_STR		"ATC - by Ed James"

#define PI			3.14159654

#define LOWFUEL			15

#define REALLOC			10

#define SGN(x)			((x < 0) ? -1 : ((x > 0) ? 1 : 0))
#define ABS(x)			((x < 0) ? -(x) : (x))
#define DIR_FROM_DXDY(dx,dy)	((int) (atan2((double)(dy), (double)(dx)) \
				* MAXDIR / (2 * PI) + 2.5 + MAXDIR) % MAXDIR)

#define MAXDIR		8

#define D_LEFT		1
#define D_RIGHT		2
#define D_UP		3
#define D_DOWN		4

#define T_NODEST	0
#define T_BEACON	1
#define T_EXIT		2
#define T_AIRPORT	3

#define S_NONE		0
#define S_GONE		1
#define S_MARKED	2
#define S_UNMARKED	3
#define S_IGNORED	4

#define INPUT_LINES	3
#define PLANE_COLS	20
