#ifndef lint
static char sccsid[] = "@(#)logent.c	5.5 (Berkeley) %G%";
#endif

#include "uucp.h"
#ifdef BSD4_2
#include <sys/time.h>
#else
#include <time.h>
#endif
#if defined(USG) || defined(BSD4_2)
#include <fcntl.h>
#endif

static FILE *Lp = NULL;
static FILE *Sp = NULL;
static Ltried = 0;
static Stried = 0;

/*LINTLIBRARY*/

/*
 *	make log entry
 */
logent(text, status)
char *text, *status;
{
#ifdef LOGBYSITE
	char lfile[MAXFULLNAME];
	static char LogRmtname[64];
#endif LOGBYSITE
	if (Rmtname[0] == '\0')
		strcpy(Rmtname, Myname);
	/* Open the log file if necessary */
#ifdef LOGBYSITE
	if (strcmp(Rmtname, LogRmtname)) {
		if (Lp != NULL)
			fclose(Lp);
		Lp = NULL;
		Ltried = 0;
	}
#endif LOGBYSITE
	if (Lp == NULL) {
		if (!Ltried) {
			int savemask;
#ifdef F_SETFL
			int flags;
#endif
			savemask = umask(LOGMASK);
#ifdef LOGBYSITE
			(void) sprintf(lfile, "%s/%s/%s", LOGBYSITE, Progname, Rmtname);
			strcpy(LogRmtname, Rmtname);
			Lp = fopen (lfile, "a");
#else !LOGBYSITE
			Lp = fopen (LOGFILE, "a");
#endif LOGBYSITE
			umask(savemask);
#ifdef F_SETFL
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

/*
 *	make a log entry
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
	time(&clock);
	tp = localtime(&clock);
	fprintf(fp, "%s %s ", User, Rmtname);
#ifdef USG
	fprintf(fp, "(%d/%d-%2.2d:%2.2d-%d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#else !USG
	fprintf(fp, "(%d/%d-%02d:%02d-%d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
#endif !USG
	fprintf(fp, "%s (%s)\n", status, text);

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
		fprintf(stderr, "%s (%s)\n", status, text);
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
	Ltried = 0;

	if (Sp != NULL)
		fclose (Sp);
	Sp = NULL;
	Stried = 0;
}


/*
 *	make system log entry
 */
syslog(text)
char *text;
{
	register struct tm *tp;
	extern struct tm *localtime();
	struct timeb clock;
#ifdef LOGBYSITE
	char lfile[MAXFULLNAME];
	static char SLogRmtname[64];

	if (strcmp(Rmtname, SLogRmtname)) {
		if (Sp != NULL)
			fclose(Sp);
		Sp = NULL;
		Stried = 0;
	}
#endif LOGBYSITE
	if (Sp == NULL) {
		if (!Stried) {
			int savemask;
#ifdef F_SETFL
			int flags;
#endif F_SETFL
			savemask = umask(LOGMASK);
#ifdef LOGBYSITE
			(void) sprintf(lfile, "%s/xferstats/%s", LOGBYSITE, Rmtname);
			strcpy(SLogRmtname, Rmtname);
			Sp = fopen (lfile, "a");
#else !LOGBYSITE
			Sp = fopen (SYSLOG, "a");
#endif LOGBYSITE
			umask(savemask);
#ifdef F_SETFL
			flags = fcntl(fileno(Sp), F_GETFL, 0);
			fcntl(fileno(Sp), F_SETFL, flags|O_APPEND);
#endif F_SETFL

		}
		Stried = 1;
		if (Sp == NULL)
			return;
		fioclex(fileno(Sp));
	}

#ifdef USG
	time(&clock.time);
	clock.millitm = 0;
#else !USG
	ftime(&clock);
#endif !USG
	tp = localtime(&clock.time);

	fprintf(Sp, "%s %s ", User, Rmtname);
#ifdef USG
	fprintf(Sp, "(%d/%d-%2.2d:%2.2d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
	fprintf(Sp, "(%ld) %s\n", clock.time, text);
#else !USG
	fprintf(Sp, "(%d/%d-%02d:%02d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
	fprintf(Sp, "(%ld.%02u) %s\n", clock.time, clock.millitm/10, text);
#endif !USG

	/* Position at end and flush */
#ifndef F_SETFL
	lseek (fileno(Sp), (long)0, 2);
#endif F_SETFL
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
