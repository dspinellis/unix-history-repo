/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)startdaemon.c	5.8 (Berkeley) %G%";
#endif /* not lint */

/*
 * Tell the printer daemon that there are new files in the spool directory.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include "lp.local.h"
#include "pathnames.h"

startdaemon(printer)
	char *printer;
{
	struct sockaddr_un un;
	register int s, n;
	char buf[BUFSIZ];
	static void perr();

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	if (s < 0) {
		perr("socket");
		return(0);
	}
	un.sun_family = AF_UNIX;
	strcpy(un.sun_path, _PATH_SOCKETNAME);
	if (connect(s, (struct sockaddr *)&un, strlen(un.sun_path) + 2) < 0) {
		perr("connect");
		(void) close(s);
		return(0);
	}
	(void) sprintf(buf, "\1%s\n", printer);
	n = strlen(buf);
	if (write(s, buf, n) != n) {
		perr("write");
		(void) close(s);
		return(0);
	}
	if (read(s, buf, 1) == 1) {
		if (buf[0] == '\0') {		/* everything is OK */
			(void) close(s);
			return(1);
		}
		putchar(buf[0]);
	}
	while ((n = read(s, buf, sizeof(buf))) > 0)
		fwrite(buf, 1, n, stdout);
	(void) close(s);
	return(0);
}

static void
perr(msg)
	char *msg;
{
	extern int errno;
	extern char *name;
	char *strerror();

	(void)printf("%s: %s: %s\n", name, msg, strerror(errno));
}
