/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)logout.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <utmp.h>

typedef struct utmp UTMP;

logout(line)
	register char *line;
{
	register int fd;
	UTMP ut;
	int rval;
	off_t lseek();
	time_t time();

	if ((fd = open(_PATH_UTMP, O_RDWR)) < 0)
		return(0);
	rval = 0;
	while (read(fd, (char *)&ut, sizeof(UTMP)) == sizeof(UTMP)) {
		if (!ut.ut_name[0] || strncmp(ut.ut_line, line, UT_LINESIZE))
			continue;
		bzero(ut.ut_name, UT_NAMESIZE);
		bzero(ut.ut_host, UT_HOSTSIZE);
		(void)time(&ut.ut_time);
		(void)lseek(fd, -(long)sizeof(UTMP), L_INCR);
		(void)write(fd, (char *)&ut, sizeof(UTMP));
		rval = 1;
	}
	(void)close(fd);
	return(rval);
}
