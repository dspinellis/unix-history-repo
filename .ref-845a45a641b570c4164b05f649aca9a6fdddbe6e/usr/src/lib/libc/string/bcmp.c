/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bcmp.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>

/*
 * bcmp -- vax cmpc3 instruction
 */
int
bcmp(b1, b2, length)
	const void *b1, *b2;
	register size_t length;
{
	register char *p1, *p2;

	if (length == 0)
		return(0);
	p1 = (char *)b1;
	p2 = (char *)b2;
	do
		if (*p1++ != *p2++)
			break;
	while (--length);
	return(length);
}
