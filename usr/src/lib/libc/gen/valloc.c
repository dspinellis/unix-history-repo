/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)valloc.c	5.2 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

char	*malloc();

char *
valloc(i)
	int i;
{
	int valsiz = getpagesize(), j;
	char *cp = malloc(i + (valsiz-1));

	j = ((int)cp + (valsiz-1)) &~ (valsiz-1);
	return ((char *)j);
}
