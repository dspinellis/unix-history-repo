/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)waitpid.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/resource.h>

pid_t
#if __STDC__
waitpid(pid_t pid, int *istat, int options)
#else
waitpid(pid, istat, options)
	pid_t pid;
	int *istat;
	int options;
#endif
{
	return (wait4(pid, istat, options, (struct rusage *)0));
}
