	/*  @(#)logent	2.1  5/18/79  13:27:52  */
#include "uucp.h"
#include <sys/types.h>
#include <time.h>

char Slogent[] = "@(#)logent	2.1";


char Tmplog[MAXFULLNAME] = "";
FILE *Lp = NULL;

/*******
 *	logent(text, status)	make log entry
 *	char *text, *status;
 *
 *	return code - none
 */

logent(text, status)
char *text, *status;
{
	int n;
	FILE *fp;
	if (Lp != NULL) {
		/*  make entry in existing temp log file  */
		mlogent(Lp, status, text);
		return;
	}

	if (ulockf(LOGLOCK, 10l) == 0) {
		if ((fp = fopen(LOGFILE, "a")) == NULL) {
			rmlock(LOGLOCK);
		}
		else {
			mlogent(fp, status, text);
			fclose(fp);
			rmlock(LOGLOCK);
			return;
		}
	}

	/*  make a temp log file  */
	for (n = 0; n < 10; n++) {
		sprintf(Tmplog, "%s/LOG.%05d.%1d", LOGDIR, getpid(), n);
		if (access(Tmplog, 0) == -1)
			break;
	}
	if ((Lp = fopen(Tmplog, "w")) == NULL)
		return;
	chmod(Tmplog, 0222);
	setbuf(Lp, NULL);
	mlogent(Lp, status, text);
	return;
}

/***
 *	mlogent(fp, status, text)  - make a log entry
 */

mlogent(fp, status, text)
char *text, *status;
FILE *fp;
{
	static pid = 0;
	struct tm *tp;
	extern struct tm *localtime();
	time_t clock;

	if (!pid)
		pid = getpid();
	time(&clock);
	tp = localtime(&clock);
	fprintf(fp, "%s %s ", User, Rmtname);
	fprintf(fp, "(%d/%d-%d:%d-%d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min, pid);
	fprintf(fp, "%s (%s)\n", status, text);
	return;
}

/***
 *	logcls()	close log file
 *
 *	return codes:  none
 */

logcls()
{
	if (Lp != NULL) {
		fclose(Lp);
		chmod(Tmplog, 0666);
	}
	return;
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
	struct tm *tp;
	extern struct tm *localtime();
	time_t clock;
	FILE *fp;

	time(&clock);
	tp = localtime(&clock);
	fp = fopen(SYSLOG, "a");
	if (fp == NULL)
		return;
	fprintf(fp, "%s %s ", User, Rmtname);
	fprintf(fp, "(%d/%d-%d:%d) ", tp->tm_mon + 1,
		tp->tm_mday, tp->tm_hour, tp->tm_min);
	fprintf(fp, "%s\n", text);
	fclose(fp);
	return;
}
