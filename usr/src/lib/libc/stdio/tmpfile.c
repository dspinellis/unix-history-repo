/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)tmpfile.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <paths.h>

FILE *
tmpfile()
{
	sigset_t set, oset;
	FILE *fp;
	int fd, sverrno;
#define	TRAILER	"tmp.XXXXXX"
	char buf[sizeof(_PATH_TMP) + sizeof(TRAILER)];

	bcopy(_PATH_TMP, buf, sizeof(_PATH_TMP) - 1);
	bcopy(TRAILER, buf + sizeof(_PATH_TMP) - 1, sizeof(TRAILER));

	sigfillset(&set);
	(void)sigprocmask(SIG_BLOCK, &set, &oset);

	fd = mkstemp(buf);
	if (fd != -1)
		(void)unlink(buf);

	(void)sigprocmask(SIG_SETMASK, &oset, NULL);

	if (fd == -1)
		return (NULL);

	if (!(fp = fdopen(fd, "w+"))) {
		sverrno = errno;
		(void)close(fd);
		errno = sverrno;
		return (NULL);
	}
	return (fp);
}
