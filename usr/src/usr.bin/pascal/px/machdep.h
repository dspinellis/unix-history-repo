/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)machdep.h	5.1 (Berkeley) %G%
 */

#ifdef ADDR32
#define pushaddr push4
#define popaddr (char *)pop4
#endif ADDR32
#ifdef ADDR16
#define pushaddr push2
#define popaddr (char *)pop2
#endif ADDR16
