/*
 * Copyright (c) 1986, 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)error.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>

#include "bug.h"
#include "extern.h"

static short	err_redir;			/* stderr redirected */

/*
 * seterr --
 *	redirect stderr for error processing
 */
void
seterr()
{
	if (!freopen(ERROR_FILE, "a", stderr))
		error("can't open error file %s.", ERROR_FILE);
	err_redir = YES;
}

/*
 * error --
 *	write errors to log file and die
 */
void
error(fmt, arg)
	register char	*fmt,
			*arg;
{
	static char	logmsg[MAXLINELEN];	/* syslog message */

	if (err_redir) {
		/* don't combine these, "fmt" may not require "arg" */
		fprintf(stderr, "\t%s\n\t", tmpname);
		fprintf(stderr, fmt, arg);
		fputc('\n', stderr);
	}
	else {
		sprintf(logmsg, "bugfiler: %s", fmt);
		syslog(LOG_ERR, logmsg, arg);
	}
#ifdef METOO
	exit(ERR);
#else
	exit(OK);
#endif
}
