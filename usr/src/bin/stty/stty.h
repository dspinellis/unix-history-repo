/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stty.h	5.1 (Berkeley) %G%
 */

struct modes {
	char *name;
	long set;
	long unset;
};

struct cchar {
	char *name;
	int sub;
	u_char def;
};

enum FMT { NOTSET, GFLAG, BSD, POSIX };

#define	LINELENGTH	72
