/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcat.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>

char *
strcat(s, append)
	register char *s;
	register const char *append;
{
	char *save = s;

	for (; *s; ++s);
	while (*s++ = *append++);
	return(save);
}
