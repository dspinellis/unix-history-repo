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
static char sccsid[] = "@(#)strtok.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

char *
strtok(s, sep)
	register char *s, *sep;
{
	register char *p;
	register c;
	static char *lasts;

	if (s == 0)
		s = lasts;
	if (s == 0)
		return (0);

	while (c = *s) {
		if (!index(sep, c))
			break;
		s++;
	}

	if (c == '\0') {
		lasts = 0;
		return (0);
	}

	for (p = s; c = *++p; )
		if (index(sep, c))
			break;

	if (c == '\0')
		lasts = 0;
	else {
		*p++ = '\0';
		lasts = p;
	}
	return (s);
}
