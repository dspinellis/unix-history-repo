/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcasecmp.c	5.1 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <ctype.h>

/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 * case insensitive
 */
strcasecmp(s1, s2)
	register char	*s1, *s2;
{
	register char	c1, c2;

	for (;; ++s1, ++s2) {
		c2 = isupper(*s2) ? tolower(*s2) : *s2;
		c1 = isupper(*s1) ? tolower(*s1) : *s1;
		if (c1 != c2)
			return(c1 - c2);
		if (!c1)
			return(0);
	}
}

/*
 * Compare strings (at most n bytes):  s1>s2: >0  s1==s2: 0  s1<s2: <0
 * case insensitive
 */
strcasencmp(s1, s2, n)
	register char	*s1, *s2;
	register int	n;
{
	register char	c1, c2;

	for (; n; ++s1, ++s2, --n) {
		c2 = isupper(*s2) ? tolower(*s2) : *s2;
		c1 = isupper(*s1) ? tolower(*s1) : *s1;
		if (c1 != c2)
			return(c1 - c2);
		if (!c1)
			return(0);
	}
	return(0);
}
