#ifndef lint
static char sccsid[] = "@(#)gnxseq.c	5.4 (Berkeley) %G%";
#endif

#include "uucp.h"
#ifdef BSD4_2
#include <sys/time.h>
#else sane
#include <time.h>
#endif sane

/*LINTLIBRARY*/

#ifdef GNXSEQ

/*
 *	get next conversation sequence number
 *
 *	return - 0 no entry | >0 sequence number
 */

gnxseq(rmtname)
char *rmtname;
{
	int count = 0, ct, ret, i;
	register struct tm *tp;
	extern struct tm *localtime();
	time_t clock;
	register FILE *fp0, *fp1;
	char buf[BUFSIZ], name[NAMESIZE];

	if (access(SQFILE, 0) != 0)	/* nothing to do here */
		return(0);

	for (i = 0; i < 5; i++) {
		if ((ret = ulockf(SQLOCK, (time_t)  SQTIME)) == 0)
			break;
		sleep(5);
	}
	if (ret != 0) {
		logent("CAN'T LOCK", SQLOCK);
		DEBUG(4, "can't lock %s\n", SQLOCK);
		return(0);
	}
	if ((fp0 = fopen(SQFILE, "r")) == NULL)
		return(0);
	if ((fp1 = fopen(SQTMP, "w")) == NULL) {
		fclose(fp0);
		return(0);
	}
	chmod(SQTMP, 0400);

	while (fgets(buf, BUFSIZ, fp0) != NULL) {
		ret = sscanf(buf, "%s%d", name, &ct);
		if (ret < 2)
			ct = 0;
		name[MAXBASENAME] = '\0';
		if (ct > 9998)
			ct = 0;
		if (strncmp(rmtname, name, MAXBASENAME) != SAME) {
			fputs(buf, fp1);
			continue;
		}

		/*  found name  */
		count = ++ct;
		time(&clock);
		tp = localtime(&clock);
#ifdef USG
		fprintf(fp1, "%s %d %d/%d-%2.2d:%2.2d\n", name, ct,
		  tp->tm_mon + 1, tp->tm_mday, tp->tm_hour,
		  tp->tm_min);
#endif
#ifndef USG
		fprintf(fp1, "%s %d %d/%d-%02d:%02d\n", name, ct,
		  tp->tm_mon + 1, tp->tm_mday, tp->tm_hour,
		  tp->tm_min);
#endif
		while (fgets(buf, BUFSIZ, fp0) != NULL)
			fputs(buf, fp1);
	}
	fclose(fp0);
	fclose(fp1);
	if (count == 0) {
		rmlock(SQLOCK);
		unlink(SQTMP);
	}
	return(count);
}


/***
 *	cmtseq()	commit sequence update
 *
 *	return  0  ok | other - link failed
 */

cmtseq()
{
	register int ret;

	if ((ret = access(SQTMP, 04)) != 0) {
		rmlock(SQLOCK);
		return(0);
	}
	unlink(SQFILE);
	ret = link(SQTMP, SQFILE);
	unlink(SQTMP);
	rmlock(SQLOCK);
	return(ret);
}

/***
 *	ulkseq()	unlock sequence file
 */

ulkseq()
{
	unlink(SQTMP);
	rmlock(SQLOCK);
}
#endif GNXSEQ
