/*-
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)prefix.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*LINTLIBRARY*/

/*
 *	check s2 for prefix s1
 *
 *	return 0 - !=
 *	return 1 - == 
 */

prefix(s1, s2)
register char *s1, *s2;
{
	register char c;

	while ((c = *s1++) == *s2++)
		if (c == '\0')
			return 1;
	return c == '\0';
}

/*
 *	check s2 for prefix s1 with a wildcard character ?
 *
 *	return 0 - !=
 *	return 1 - == 
 */

wprefix(s1, s2)
register char *s1, *s2;
{
	register char c;

	while ((c = *s1++) != '\0')
		if (*s2 == '\0'  ||  (c != *s2++  &&  c != '?'))
			return 0;
	return 1;
}
