#ifndef lint
static char sccsid[] = "@(#)logent.c	5.3 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/types.h>
#ifdef BSD4_2
#include <sys/time.h>
#else
#include <time.h>
#endif
#if defined(USG) || defined(BSD4_2)
#include <fcntl.h>
#endif

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
 * Under USG UNIX & 4.2BSD, the files are opened with "guaranteed append to end"
 * and the lseeks are removed.
 */


static FILE *Lp = NULL;
static FILE *Sp = NULL;
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
#if defined(USG) || defined(BSD4_2)
			int flags;
#endif
			savemask = umask(LOGMASK);
			Lp = fopen (LOGFILE, "a");
			umask(savemask);
#if defined(USG) || defined(BSD4_2)
			flags = fcntl(fileno(Lp), F_GETFL, 0);
			fcntl(fileno(Lp), F_SETFL, flags|O_APPEND);
#endif
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

	if (text == NULL)
		text = "";
	if (status == NULL)
		status = "";
	if (!pid)
		pid = getpid();
	if (Rmtname[0] == '\0')
		strcpy(Rmtname, Myname);
	time(&clock);
	tp = localtime(&clock);
	fprintf(fp, "%s %s ", User, Rmtname);
#ifdef USG
	fprintf(fp, "(%d/%d-%2.2d:%2.2d-%d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#endif
#ifndef USG
	fprintf(fp, "(%d/%d-%02d:%02d-%d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#endif
	fprintf(fp, "%s (%s)\n", status, text);

	/* Since it's buffered */
#ifndef USG
	lseek (fileno(fp), (long)0, 2);
#endif
	fflush (fp);
	if (Debug) {
		fprintf(stderr, "%s %s ", User, Rmtname);
#ifdef USG
		fprintf(stderr, "(%d/%d-%2.2d:%2.2d-%d) ", tp->tm_mon + 1,
			tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#endif
#ifndef USG
		fprintf(stderr, "(%d/%d-%02d:%02d-%d) ", tp->tm_mon + 1,
			tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#endif
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
#if defined(USG) || defined(BSD4_2)
			int flags;
#endif
			savemask = umask(LOGMASK);
			Sp = fopen(SYSLOG, "a");
			umask(savemask);
#if defined(USG) || defined(BSD4_2)
			flags = fcntl(fileno(Sp), F_GETFL, 0);
			fcntl(fileno(Sp), F_SETFL, flags|O_APPEND);
#endif
		}
		Stried = 1;
		if (Sp == NULL)
			return;
		fioclex(fileno(Sp));
	}

	time(&clock);
	tp = localtime(&clock);

	fprintf(Sp, "%s %s ", User, Rmtname);
#ifdef USG
	fprintf(Sp, "(%d/%d-%2.2d:%2.2d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
#endif
#ifndef USG
	fprintf(Sp, "(%d/%d-%02d:%02d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
#endif
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
#ifndef	USG
#include <sgtty.h>
#endif

fioclex(fd)
int fd;
{
	register int ret;

#if defined(USG) || defined(BSD4_2)
	ret = fcntl(fd, F_SETFD, 1);	/* Steve Bellovin says this does it */
#else
	ret = ioctl(fd, FIOCLEX, STBNULL);
#endif
	if (ret)
		DEBUG(2, "CAN'T FIOCLEX %d\n", fd);
}
