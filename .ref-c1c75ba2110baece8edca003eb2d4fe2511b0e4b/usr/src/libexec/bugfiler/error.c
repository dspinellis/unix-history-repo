/*
 * Copyright (c) 1986, 1987 Regents of the University of California.
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

#ifndef lint
static char sccsid[] = "@(#)error.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <bug.h>
#include <syslog.h>
#include <stdio.h>

static short	err_redir;			/* stderr redirected */

/*
 * seterr --
 *	redirect stderr for error processing
 */
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
error(fmt, arg)
	register char	*fmt,
			*arg;
{
	static char	logmsg[MAXLINELEN];	/* syslog message */
	char	*strcpy(), *strcat();

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
