/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)daemon.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/fcntl.h>
#include <unistd.h>
#include <paths.h>

daemon(nochdir, noclose)
	int nochdir, noclose;
{
	int cpid;

	if ((cpid = fork()) == -1)
		return (-1);
	if (cpid)
		exit(0);
	(void) setsid();
	if (!nochdir)
		(void) chdir("/");
	if (!noclose) {
		int devnull = open(_PATH_DEVNULL, O_RDWR, 0);

		if (devnull != -1) {
			(void) dup2(devnull, STDIN_FILENO);
			(void) dup2(devnull, STDOUT_FILENO);
			(void) dup2(devnull, STDERR_FILENO);
			if (devnull > 2)
				(void) close(devnull);
		}
	}
}
