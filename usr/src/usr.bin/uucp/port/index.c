/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)index.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include <stdio.h>

/*
 *	return pointer to character c
 *
 *	return codes:
 *		NULL  -  character not found
 *		pointer  -  pointer to character
 */

char *
index(str, c)
register char c, *str;
{
	for (; *str != '\0'; str++) {
		if (*str == c)
			return str;
	}

	return NULL;
}
