/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)bcmp.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <string.h>

/*
 * bcmp -- vax cmpc3 instruction
 */
bcmp(b1, b2, length)
	register char *b1, *b2;
	register size_t length;
{

	if (length == 0)
		return(0);
	do
		if (*b1++ != *b2++)
			break;
	while (--length);
	return(length);
}
