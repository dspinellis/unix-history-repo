/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)error.c	5.1 (Berkeley) 86/11/25";
#endif not lint

#include <syslog.h>
#include <stdio.h>
#include <bug.h>

extern char	*distf,				/* redist temp file */
		tmpname[];			/* temporary file used */

short	made_dist;				/* if dist file made */

static short	err_redir;			/* stderr redirected */

/*
 * seterr --
 *	redirect stderr for error processing
 */
seterr()
{
	if (!freopen(ERROR_FILE,"a",stderr))
		error("unable to open error file %s.\n",ERROR_FILE);
	err_redir = YES;
}

/*
 * error --
 *	write errors to log file and die
 */
error(fmt,arg)
register char	*fmt,
		*arg;
{
	static char	logmsg[MAXLINELEN];	/* syslog message */
	char	*strcpy(), *strcat();

	if (err_redir) {
		/* don't combine these, "fmt" may not require "arg" */
		fputc('\t',stderr);
		fprintf(stderr,fmt,arg);
		fprintf(stderr,"\n\ttemporary file is %s.\n",tmpname);
	}
	else {
		strcat(strcpy(logmsg,"bugfiler: "),fmt);
		syslog(LOG_ERR,logmsg,arg);
	}
	if (made_dist)		/* unlink redist file if necessary */
		unlink(distf);
#ifdef METOO
	exit(ERR);
#else !METOO
	exit(OK);
#endif METOO
}
