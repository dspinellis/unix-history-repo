/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)alias.h	8.1 (Berkeley) %G%
 */

#define ALIASINUSE	1

struct alias {
	struct alias *next;
	char *name;
	char *val;
	int flag;
};

struct alias *lookupalias();
