/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)rindex_.c	5.3 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * find last occurrence of substring in string
 *
 * calling sequence:
 *	character*(*) substr, string
 *	indx = rindex (string, substr)
 * where:
 *	indx will be the index of the first character of the last occurence
 *	of substr in string, or zero if not found.
 */

long rindex_(str, substr, slen, sublen)
char *str, *substr; long slen, sublen;
{
	register char	*p = str + (slen - sublen);
	register char	*p1, *p2;
	register int	len;

	if (sublen == 0)
		return(0L);
	while (p >= str) {
		p1 = p;
		p2 = substr;
		len = sublen;
		while ( *p1++ == *p2++ && --len > 0) ;
		if ( len <= 0 )
			return((long)(++p - str));
		p--;
	}
	return(0L);
}
