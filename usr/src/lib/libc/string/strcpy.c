/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcpy.c	5.7 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/cdefs.h>
#include <string.h>

char *
strcpy(to, from)
	register char *to;
	register const char *from;
{
	char *save = to;

	for (; *to = *from; ++from, ++to);
	return(save);
}
