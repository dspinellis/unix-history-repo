/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)memset.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

char *
memset(s, c, n)
	register char *s;
	register c, n;
{
	register char *p = s;

	while (--n >= 0)
		*s++ = c;

	return (p);
}
