/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)machdep.h	8.1 (Berkeley) %G%
 */

/*
 * Most machines require the value returned from malloc to be aligned
 * in some way.  The following macro will get this right on many machines.
 */

#ifndef ALIGN
union align {
	int i;
	char *cp;
};

#define ALIGN(nbytes)	((nbytes) + sizeof(union align) - 1 &~ (sizeof(union align) - 1))
#endif
