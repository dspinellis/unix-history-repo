/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)logent.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include "uucp.h"
#ifdef BSD4_2
#include <sys/time.h>
#else
#include <time.h>
#endif
#if defined(USG) || defined(BSD4_2)
#include <fcntl.h>
#endif

extern int errno;
extern int sys_nerr;
extern const char *const sys_errlist[];

static FILE *Lp = NULL;
static FILE *Sp = NULL;
#ifndef USE_SYSLOG
static FILE *Ep = NULL;
#endif /* !USE_SYSLOG */
static int pid = 0;

/*LINTLIBRARY*/

/*
 *	make log entry
 */
FILE *
get_logfd(pname, logfilename)
char *pname;
char *logfilename;
{
	FILE *fp;
	int savemask;
#ifdef LOGBYSITE
	char lfile[MAXFULLNAME];
#endif LOGBYSITE

	savemask = umask(LOGMASK);
#ifdef LOGBYSITE
	if (pname != NULL) {
		(void) sprintf(lfile, "%s/%s/%s", LOGBYSITE, pname, Rmtname);
		logfilename = lfile;
	}
#endif LOGBYSITE
	fp = fopen(logfilename, "a");
	umask(savemask);
	if (fp) {
#ifdef		F_SETFL
		int flags;
		flags = fcntl(fileno(fp), F_GETFL, 0);
		fcntl(fileno(Lp), F_SETFL, flags|O_APPEND);
#endif		/* F_SETFL */
		fioclex(fileno(fp));
	} else /* we really want to log this, but it's the logging that failed*/
		perror(logfilename);
	return fp;
}

/*
 *	make a log entry
 */
mlogent(fp, status, text)
char *text, *status;
register FILE *fp;
{
	register struct tm *tp;
	extern struct tm *localtime();

	if (text == NULL)
		text = "";
	if (status == NULL)
		status = "";
	if (pid == 0)
		pid = getpid();
#ifdef USG
	time(&Now.time);
	Now.millitm = 0;
#else !USG
	ftime(&Now);
#endif !USG
	tp = localtime(&Now.time);
#ifdef USG
	fprintf(fp, "%s %s (%d/%d-%2.2d:%2.2d-%d) ",
#else !USG
	fprintf(fp, "%s %s (%d/%d-%02d:%02d-%d) ",
#endif !USG
		User, Rmtname, tp->tm_mon + 1, tp->tm_mday,
		tp->tm_hour, tp->tm_min, pid);
	fprintf(fp, "%s %s\n", status, text);

	/* Since it's buffered */
#ifndef F_SETFL
	lseek (fileno(fp), (long)0, 2);
#endif !F_SETFL
	fflush (fp);
	if (Debug) {
		fprintf(stderr, "%s %s ", User, Rmtname);
#ifdef USG
		fprintf(stderr, "(%d/%d-%2.2d:%2.2d-%d) ", tp->tm_mon + 1,
			tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#else !USG
		fprintf(stderr, "(%d/%d-%02d:%02d-%d) ", tp->tm_mon + 1,
			tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#endif !USG
		fprintf(stderr, "%s %s\n", status, text);
	}
}

/*
 *	close log file
 */
logcls()
{
	if (Lp != NULL)
		fclose(Lp);
	Lp = NULL;

	if (Sp != NULL)
		fclose (Sp);
	Sp = NULL;
#ifndef USE_SYSLOG
	if (Ep != NULL)
		fclose (Ep);
	Ep = NULL;
#endif /* !USE_SYSLOG */
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

logent(text, status)
char *text, *status;
{
	if (Lp == NULL)
		Lp = get_logfd(Progname, LOGFILE);

	mlogent(Lp, status, text);
}

/*
 *	make system log entry
 */
log_xferstats(text)
char *text;
{
	char tbuf[BUFSIZ];
	if (Sp == NULL)
		Sp = get_logfd("xferstats", SYSLOG);
	sprintf(tbuf, "(%ld.%02u)", Now.time, Now.millitm/10);
	mlogent(Sp, tbuf, text);
}

#ifndef USE_SYSLOG
/*
 * This is for sites that don't have a decent syslog() in their library
 * This routine would be a lot simpler if syslog() didn't permit %m
 * (or if printf did!)
 */
syslog(priority, format, p0, p1, p2, p3, p4)
int priority;
char *format;
{
	char nformat[BUFSIZ], sbuf[BUFSIZ];
	register char *s, *d;
	register int c;
	long now;

	s = format;
	d = nformat;
	while ((c = *s++) != '\0' && c != '\n' && d < &nformat[BUFSIZ]) {
		if (c != '%') {
			*d++ = c;
			continue;
		}
		if ((c = *s++) != 'm') {
			*d++ = '%';
			*d++ = c;
			continue;
		}
		if ((unsigned)errno > sys_nerr)
			sprintf(d, "error %d", errno);
		else
			strcpy(d, sys_errlist[errno]);
		d += strlen(d);
	}
	*d = '\0';

	if (Ep == NULL)
		Ep = get_logfd(NULL, ERRLOG);
	sprintf(sbuf, nformat, p0, p1, p2, p3, p4);
	mlogent(Ep, sbuf, "");
}
#endif /* !USE_SYSLOG */
