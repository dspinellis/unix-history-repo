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
static char sccsid[] = "@(#)remove.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <unistd.h>
#include <stdio.h>

remove(file)
	const char *file;
{
	return (unlink(file));
}
