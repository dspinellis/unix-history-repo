#include "uucp.h"
#include <sys/types.h>
#include <time.h>



/*******
 *	gnxseq(rmtname)		get next conversation sequence number
 *	char *rmtname;
 *
 *	return - 0 no entry | >0 sequence number
 */

gnxseq(rmtname)
char *rmtname;
{
	int count = 0, ct, ret, i;
	struct tm *tp;
	extern struct tm *localtime();
	time_t clock;
	FILE *fp0, *fp1;
	char buf[BUFSIZ], name[NAMESIZE];

	for (i = 0; i < 5; i++) {
		if ((ret = ulockf(SQLOCK, SQTIME)) == 0)
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
		name[7] = '\0';
		if (ct > 9998)
			ct = 0;
		if (strcmp(rmtname, name) != SAME) {
			fputs(buf, fp1);
			continue;
		}

		/*  found name  */
		count = ++ct;
		time(&clock);
		tp = localtime(&clock);
		fprintf(fp1, "%s %d %d/%d-%d:%d\n", name, ct,
		  tp->tm_mon + 1, tp->tm_mday, tp->tm_hour,
		  tp->tm_min);
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
	int ret;

	if ((ret = access(SQTMP, 0400)) != 0) {
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
