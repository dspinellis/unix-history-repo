/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys5 compat routine
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcspn.c	5.2 (Berkeley) 86/03/09";
#endif

strcspn(s, set)
	register char *s, *set;
{
	register n = 0;
	register char *p;
	register c;

	while (c = *s++) {
		for (p = set; *p; p++)
			if (c == *p)
				break;
		if (*p)
			return (n);
		n++;
	}
	return (n);
}
