/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)daemon.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/file.h>

daemon()
{
	int cpid;

	if ((cpid = fork()) == -1)
		return (-1);
	if (cpid)
		exit(0);

	(void) setsid();
	(void) chdir("/");
	(void) close(0);
	(void) close(1);
	(void) close(2);
	(void) open("/", O_RDONLY, 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
}
