/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)labs.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>

long
labs(j)
	long j;
{
	return(j < 0 ? -j : j);
}
