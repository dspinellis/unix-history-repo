/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)logwtmp.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <utmp.h>

#define	WTMPFILE	"/usr/adm/wtmp"

logwtmp(line)
	char *line;
{
	struct utmp ut;
	int fd;
	time_t time();
	char *strncpy();

	if ((fd = open(WTMPFILE, O_WRONLY|O_APPEND, 0)) < 0)
		return;
	bzero(ut.ut_name, sizeof(ut.ut_name));
	bzero(ut.ut_host, sizeof(ut.ut_host));
	(void)strncpy(ut.ut_line, line, sizeof(ut.ut_line));
	(void)time(&ut.ut_time);
	(void)write(fd, (char *)&ut, sizeof(struct utmp));
	(void)close(fd);
}
