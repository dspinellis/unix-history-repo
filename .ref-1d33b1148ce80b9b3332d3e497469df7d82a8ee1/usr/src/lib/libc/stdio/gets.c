/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)gets.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <unistd.h>
#include <stdio.h>

char *
gets(buf)
	char *buf;
{
	register int c;
	register char *s;
	static int warned;
	static char w[] =
	    "warning: this program uses gets(), which is unsafe.\r\n";

	if (!warned) {
		(void) write(STDERR_FILENO, w, sizeof(w) - 1);
		warned = 1;
	}
	for (s = buf; (c = getchar()) != '\n';)
		if (c == EOF)
			if (s == buf)
				return (NULL);
			else
				break;
		else
			*s++ = c;
	*s = 0;
	return (buf);
}
