/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)wwerror.c	3.8 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"

char *
wwerror()
{
	extern int errno;
	char *strerror();

	switch (wwerrno) {
	case WWE_NOERR:
		return "No error";
	case WWE_SYS:
		return strerror(errno);
	case WWE_NOMEM:
		return "Out of memory";
	case WWE_TOOMANY:
		return "Too many windows";
	case WWE_NOPTY:
		return "Out of pseudo-terminals";
	case WWE_SIZE:
		return "Bad window size";
	case WWE_BADTERM:
		return "Unknown terminal type";
	case WWE_CANTDO:
		return "Can't run window on this terminal";
	default:
		return "Unknown error";
	}
}
