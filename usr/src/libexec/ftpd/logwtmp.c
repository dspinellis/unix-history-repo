/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 */

#ifndef lint
static char sccsid[] = "@(#)logwtmp.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <utmp.h>

static int fd = -1;

/*
 * Modified version of logwtmp that holds wtmp file open
 * after first call, for use with ftp (which may chroot
 * after login, but before logout).
 */
logwtmp(line, name, host)
	char *line, *name, *host;
{
	struct utmp ut;
	struct stat buf;
	time_t time();
	char *strncpy();

	if (fd < 0 && (fd = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0)) < 0)
		return;
	if (fstat(fd, &buf) == 0) {
		(void)strncpy(ut.ut_line, line, sizeof(ut.ut_line));
		(void)strncpy(ut.ut_name, name, sizeof(ut.ut_name));
		(void)strncpy(ut.ut_host, host, sizeof(ut.ut_host));
		(void)time(&ut.ut_time);
		if (write(fd, (char *)&ut, sizeof(struct utmp)) !=
		    sizeof(struct utmp))
			(void)ftruncate(fd, buf.st_size);
	}
}
