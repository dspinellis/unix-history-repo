/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setpgrp.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <unistd.h>

#if __STDC__
setpgrp(pid_t pid, pid_t pgid)
#else
setpgrp(pid, pgid)
	pid_t pid, pgid;
#endif
{
	return(setpgid(pid, pgid));
}
