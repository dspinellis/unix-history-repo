#ifndef lint
static char sccsid[] = "@(#)assert.c	5.5 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/time.h>
#include <errno.h>

/*LINTLIBRARY*/

/*
 *	print out assetion error
 */

assert(s1, s2, i1)
char *s1, *s2;
{
	register FILE *errlog;
	register struct tm *tp;
	extern struct tm *localtime();
	time_t clock;
	int pid;

	errlog = NULL;
	if (!Debug) {
		int savemask;
		savemask = umask(LOGMASK);
		errlog = fopen(ERRLOG, "a");
		umask(savemask);
	}
	if (errlog == NULL)
		errlog = stderr;

	pid = getpid();
	fprintf(errlog, "ASSERT ERROR (%.9s)  ", Progname);
	fprintf(errlog, "pid: %d  ", pid);
	(void) time(&clock);
	tp = localtime(&clock);
#ifdef USG
	fprintf(errlog, "(%d/%d-%2.2d:%2.2d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
#endif
#ifndef USG
	fprintf(errlog, "(%d/%d-%02d:%02d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
#endif
	fprintf(errlog, "%s %s (%d)\n", s1 ? s1 : "", s2 ? s2 : "", i1);
	if (errlog != stderr)
		(void) fclose(errlog);
	return;
}
