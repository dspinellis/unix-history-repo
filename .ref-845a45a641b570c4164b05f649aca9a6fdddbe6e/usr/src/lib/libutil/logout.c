/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)logout.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/time.h>

#include <fcntl.h>
#include <utmp.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

typedef struct utmp UTMP;

int
logout(line)
	register char *line;
{
	register int fd;
	UTMP ut;
	int rval;

	if ((fd = open(_PATH_UTMP, O_RDWR, 0)) < 0)
		return(0);
	rval = 0;
	while (read(fd, &ut, sizeof(UTMP)) == sizeof(UTMP)) {
		if (!ut.ut_name[0] || strncmp(ut.ut_line, line, UT_LINESIZE))
			continue;
		bzero(ut.ut_name, UT_NAMESIZE);
		bzero(ut.ut_host, UT_HOSTSIZE);
		(void)time(&ut.ut_time);
		(void)lseek(fd, -(off_t)sizeof(UTMP), L_INCR);
		(void)write(fd, &ut, sizeof(UTMP));
		rval = 1;
	}
	(void)close(fd);
	return(rval);
}
