/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setrgid.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <unistd.h>

int
#ifdef __STDC__
setrgid(gid_t rgid)
#else
setrgid(rgid)
	int rgid;
#endif
{

	return (setregid(rgid, -1));
}
