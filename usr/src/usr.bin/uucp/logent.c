#ifndef lint
static char sccsid[] = "@(#)logent.c	5.2 (Berkeley) 7/2/83";
#endif

#include "uucp.h"
#include <sys/types.h>
#include <sys/time.h>

extern	time_t	time();

/* This logfile stuff was awful -- it did output to an
 * unbuffered stream.
 *
 * This new version just open the single logfile and writes
 * the record in the stdio buffer.  Once that's done, it
 * positions itself at the end of the file (lseek), and
 * writes the buffer out.  This could mangle things but
 * it isn't likely. -- ittvax!swatt
 *
 * If the files could be opened with "guaranteed append to end",
 * the lseeks could be removed.
 * Using fseek would be slightly cleaner,
 * but would mangle things slightly more often.
 */


FILE *Lp = NULL;
FILE *Sp = NULL;
static Ltried = 0;
static Stried = 0;

/*******
 *	logent(text, status)	make log entry
 *	char *text, *status;
 *
 *	return code - none
 */

logent(text, status)
char *text, *status;
{
	/* Open the log file if necessary */
	if (Lp == NULL) {
		if (!Ltried) {
			int savemask;
			savemask = umask(LOGMASK);
			Lp = fopen (LOGFILE, "a");
			umask(savemask);
		}
		Ltried = 1;
		if (Lp == NULL)
			return;
		fioclex(fileno(Lp));
	}

	/*  make entry in existing temp log file  */
	mlogent(Lp, status, text);
}

/***
 *	mlogent(fp, status, text)  - make a log entry
 */

mlogent(fp, status, text)
char *text, *status;
register FILE *fp;
{
	static pid = 0;
	register struct tm *tp;
	extern struct tm *localtime();
	time_t clock;

	if (!pid)
		pid = getpid();
	time(&clock);
	tp = localtime(&clock);
	fprintf(fp, "%s %s ", User, Rmtname);
	fprintf(fp, "(%d/%d-%d:%02d-%d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
	fprintf(fp, "%s (%s)\n", status, text);

	/* Since it's buffered */
	lseek (fileno(fp), (long)0, 2);
	fflush (fp);
	if (Debug > 0) {
		fprintf(stderr, "%s %s ", User, Rmtname);
		fprintf(stderr, "(%d/%d-%d:%02d-%d) ", tp->tm_mon + 1,
			tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
		fprintf(stderr, "%s (%s)\n", status, text);
	}
}

/***
 *	logcls()	close log file
 *
 *	return codes:  none
 */

logcls()
{
	if (Lp != NULL)
		fclose(Lp);
	Lp = NULL;
	Ltried = 0;

	if (Sp != NULL)
		fclose (Sp);
	Sp = NULL;
	Stried = 0;
}


/***
 *	syslog(text)	make system log entry
 *	char *text;
 *
 *	return codes - none
 */

syslog(text)
char *text;
{
	register struct tm *tp;
	extern struct tm *localtime();
	time_t clock;

	if (Sp == NULL) {
		if (!Stried) {
			int savemask;
			savemask = umask(LOGMASK);
			Sp = fopen(SYSLOG, "a");
			umask(savemask);
		}
		Stried = 1;
		if (Sp == NULL)
			return;
		fioclex(fileno(Sp));
	}
			
	time(&clock);
	tp = localtime(&clock);

	fprintf(Sp, "%s %s ", User, Rmtname);
	fprintf(Sp, "(%d/%d-%d:%02d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
	fprintf(Sp, "(%ld) %s\n", clock, text);

	/* Position at end and flush */
	lseek (fileno(Sp), (long)0, 2);
	fflush (Sp);
}

/*
 * Arrange to close fd on exec(II).
 * Otherwise unwanted file descriptors are inherited
 * by other programs.  And that may be a security hole.
 */
#ifdef SYSIII
#include <fcntl.h>
#endif
#ifndef	SYSIII
#include <sgtty.h>
#endif

fioclex(fd)
int fd;
{
	register int ret;

#ifdef	SYSIII
	ret = fcntl(fd, F_SETFD, 1);	/* Steve Bellovin says this does it */
#endif
#ifndef	SYSIII
	ret = ioctl(fd, FIOCLEX, STBNULL);
#endif
	if (ret)
		DEBUG(2, "CAN'T FIOCLEX %d\n", fd);
}
