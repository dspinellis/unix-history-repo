#ifndef lint
static char sccsid[] = "@(#)systat.c	5.5	(Berkeley) %G%";
#endif

#include "uucp.h"

#define STATNAME(f, n) sprintf(f, "%s/%s/%s", Spool, "STST", n)
#define S_SIZE 100

/*LINTLIBRARY*/

/*
 *	make system status entry
 *	return codes:  none
 */
systat(name, type, text)
char *name, *text;
int type;
{
	char filename[MAXFULLNAME], line[S_SIZE];
	int count, oldtype;
	register FILE *fp;
	time_t prestime, rtry;

	if (type == 0)
		return;
	line[0] = '\0';
	time(&prestime);
	count = 0;
	STATNAME(filename, name);

	fp = fopen(filename, "r");
	if (fp != NULL) {
		fgets(line, S_SIZE, fp);
		sscanf(line, "%d %d", &oldtype, &count);
		if (count <= 0)
			count = 0;
		fclose(fp);
		/* If merely 'wrong time', don't change existing STST */
		if (type == SS_WRONGTIME && oldtype != SS_INPROGRESS)
			return;
	}

	rtry = Retrytime;
	/* if failures repeat, don't try so often,
	 * to forstall a 'MAX RECALLS' situation.
	 */
	if (type == SS_FAIL) {
		count++;
		if (count > 5) {
			rtry = rtry * (count-5);
			if (rtry > ONEDAY/2)
				rtry = ONEDAY/2;
		}
	}


#ifdef VMS
	unlink(filename);
#endif VMS
	fp = fopen(filename, "w");
	if (fp == NULL) {
		syslog(LOG_ERR, "fopen(%s) failed: %m", filename);
		cleanup(FAIL);
	}
	fprintf(fp, "%d %d %ld %ld %s %s\n", type, count, prestime, rtry, text, name);
	fclose(fp);
}

/*
 *	remove system status entry
 *
 *	return codes:  none
 */
rmstat(name)
char *name;
{
	char filename[MAXFULLNAME];

	STATNAME(filename, name);
	unlink(filename);
}

/*
 *	check system status for call
 *
 *	return codes  0 - ok | >0 system status
 */
callok(name)
char *name;
{
	char filename[MAXFULLNAME], line[S_SIZE];
	register FILE *fp;
	time_t lasttime, prestime, retrytime;
	long t1, t2;
	int count, type;

	STATNAME(filename, name);
	fp = fopen(filename, "r");
	if (fp == NULL)
		return(SS_OK);

	if (fgets(line, S_SIZE, fp) == NULL) {
		/*  no data  */
		fclose(fp);
		unlink(filename);
		return(SS_OK);
	}

	fclose(fp);
	time(&prestime);
	sscanf(line, "%d%d%ld%ld", &type, &count, &t1, &t2);
	lasttime = t1;
	retrytime = t2;

	switch(type) {
	case SS_BADSEQ:
	case SS_CALLBACK:
	case SS_NODEVICE:
	case SS_INPROGRESS:	/*let LCK take care of it */
		return(SS_OK);

	case SS_FAIL:
		if (count > MAXRECALLS) {
			logent("MAX RECALLS", "NO CALL");
			DEBUG(4, "MAX RECALL COUNT %d\n", count);
			if (Debug) {
				logent("debugging", "continuing anyway");
				return SS_OK;
			}
			return type;
		}

		if (prestime - lasttime < retrytime) {
			logent("RETRY TIME NOT REACHED", "NO CALL");
			DEBUG(4, "RETRY TIME (%ld) NOT REACHED\n", retrytime);
			if (Debug) {
				logent("debugging", "continuing anyway");
				return SS_OK;
			}
			return type;
		}

		return SS_OK;
	default:
		return SS_OK;
	}
}
