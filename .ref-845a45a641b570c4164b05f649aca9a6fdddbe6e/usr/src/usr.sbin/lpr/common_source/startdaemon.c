/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)startdaemon.c	5.10 (Berkeley) %G%";
#endif /* not lint */


#include <sys/param.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "lp.h"
#include "pathnames.h"

static void perr __P((char *));

/*
 * Tell the printer daemon that there are new files in the spool directory.
 */

int
startdaemon(printer)
	char *printer;
{
	struct sockaddr_un un;
	register int s, n;
	char buf[BUFSIZ];

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	if (s < 0) {
		perr("socket");
		return(0);
	}
	un.sun_family = AF_UNIX;
	strcpy(un.sun_path, _PATH_SOCKETNAME);
#ifndef SUN_LEN
#define SUN_LEN(unp) (strlen((unp)->sun_path) + 2)
#endif
	if (connect(s, (struct sockaddr *)&un, SUN_LEN(&un)) < 0) {
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
	extern char *name;

	(void)printf("%s: %s: %s\n", name, msg, strerror(errno));
}
