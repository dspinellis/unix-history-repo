/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rindex_.c	5.2	6/7/85
 *
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
