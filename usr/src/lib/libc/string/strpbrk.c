/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*
 * Sys 5 compat routine...
 */

#ifndef lint
static char sccsid[] = "@(#)strpbrk.c	5.1 (Berkeley) 85/08/05";
#endif

char *
strpbrk(s, brk)
	register char *s, *brk;
{
	register char *p;
	register c;

	while (c = *s) {
		for (p = brk; *p; p++)
			if (c == *p)
				return (s);
		s++;
	}
	return (0);
}
