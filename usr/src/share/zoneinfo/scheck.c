/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Arthur David Olson of the National Cancer Institute.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)scheck.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#ifdef notdef
static char	elsieid[] = "@(#)scheck.c	8.9";
#endif

/*LINTLIBRARY*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

char *
scheck(string, format)
const char * const	string;
const char * const	format;
{
	register char *		fbuf;
	register const char *	fp;
	register char *		tp;
	register int		c;
	register char *		result;
	char			dummy;

	result = "";
	if (string == NULL || format == NULL)
		return result;
	fbuf = malloc(2 * strlen(format) + 4);
	if (fbuf == NULL)
		return result;
	fp = format;
	tp = fbuf;
	while ((*tp++ = c = *fp++) != '\0') {
		if (c != '%')
			continue;
		if (*fp == '%') {
			*tp++ = *fp++;
			continue;
		}
		*tp++ = '*';
		if (*fp == '*')
			++fp;
		while (isascii(*fp) && isdigit(*fp))
			*tp++ = *fp++;
		if (*fp == 'l' || *fp == 'h')
			*tp++ = *fp++;
		else if (*fp == '[')
			do *tp++ = *fp++;
				while (*fp != '\0' && *fp != ']');
		if ((*tp++ = *fp++) == '\0')
			break;
	}
	*(tp - 1) = '%';
	*tp++ = 'c';
	*tp = '\0';
	if (sscanf((char *)string, fbuf, &dummy) != 1)
		result = (char *) format;
	ifree(fbuf);
	return result;
}
