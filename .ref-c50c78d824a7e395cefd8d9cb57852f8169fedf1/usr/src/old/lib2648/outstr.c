/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)outstr.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Low level output routines
 */

#include "2648.h"

outstr(str)
char *str;
{
	while (*str)
		outchar(*str++);
}
