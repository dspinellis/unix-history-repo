/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)daemon.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/file.h>

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
		int devnull = open("/dev/null", O_RDWR, 0);

		if (devnull != -1) {
			(void) dup2(devnull, 0);
			(void) dup2(devnull, 1);
			(void) dup2(devnull, 2);
			if (devnull > 2)
				(void) close(devnull);
		}
	}
}
