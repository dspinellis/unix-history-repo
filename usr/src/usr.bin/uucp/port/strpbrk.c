/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)strpbrk.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*LINTLIBRARY*/

/*
 * this is like index, but takes a string as the second argument
 */
char *
strpbrk(str, chars)
register char *str, *chars;
{
	register char *cp;

	do {
		cp = chars - 1;
		while (*++cp) {
			if (*str == *cp)
				return str;
		}
	} while (*str++);
	return (char *)0;
}
