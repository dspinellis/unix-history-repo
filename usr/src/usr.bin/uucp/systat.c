static char sccsid[] = "@(#)systat.c	4.1	(Berkeley)	9/11/82";

	/*  systat 3.3  10/26/79  11:39:50  */
#include "uucp.h"
#include <sys/types.h>


#define STATNAME(f, n) sprintf(f, "%s/%s.%.7s", Spool, "STST", n)
#define S_SIZE 100

/*******
 *	systat(name, type, text)	make system status entry
 *	char *name, *text;
 *	int type.
 *
 *	return codes:  none
 */

systat(name, type, text)
char *name, *text;
int type;
{
	char filename[MAXFULLNAME], line[S_SIZE];
	int count;
	FILE *fp;
	time_t prestime;

	if (type == 0)
		return;
	line[0] = '\0';
	time(&prestime);
	count = 0;
	STATNAME(filename, name);

	fp = fopen(filename, "r");
	if (fp != NULL) {
		fgets(line, S_SIZE, fp);
		sscanf(&line[2], "%d", &count);
		if (count <= 0)
			count = 0;
		fclose(fp);
	}

	if (type == SS_FAIL)
		count++;

	fp = fopen(filename, "w");
	ASSERT(fp != NULL, "SYSTAT OPEN FAIL %s", "");
	chmod(filename, 0666);
	fprintf(fp, "%d %d %ld %s %s\n", type, count, prestime, text, name);
	fclose(fp);
	return;
}

/***
 *	rmstat(name)	remove system status entry
 *	char *name;
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

/***
 *	callok(name)	check system status for call
 *	char *name;
 *
 *	return codes  0 - ok | >0 system status
 */

callok(name)
char *name;
{
	char filename[MAXFULLNAME], line[S_SIZE];
	FILE *fp;
	time_t lasttime, prestime;
	long retrytime;
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
	sscanf(line, "%d%d%ld%ld", &type, &count, &lasttime, &retrytime);

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
			return(type);
		}

		if (prestime - lasttime < retrytime) {
			logent("RETRY TIME NOT REACHED", "NO CALL");
			DEBUG(4, "RETRY TIME (%d) NOT REACHED\n", (long)  RETRYTIME);
			return(type);
		}

		return(SS_OK);
	default:
		return(SS_OK);
	}
}
