/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setruid.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <unistd.h>

int
#ifdef __STDC__
setruid(uid_t ruid)
#else
setruid(ruid)
	int ruid;
#endif
{

	return (setreuid(ruid, -1));
}
