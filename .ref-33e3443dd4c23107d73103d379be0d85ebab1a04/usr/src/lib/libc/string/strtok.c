/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strtok.c	5.2 (Berkeley) 86/03/09";
#endif

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
