#include "uucp.h"


/*******
 *	gename(pre, sys, grade, file)	generate file name
 *	char grade, *sys, pre, *file;
 *
 *	return codes:  none
 */

gename(pre, sys, grade, file)
char pre, *sys, grade, *file;
{
	char sqnum[5];

	getseq(sqnum);
	sprintf(file, "%c.%.7s%c%.4s", pre, sys, grade, sqnum);
	DEBUG(4, "file - %s\n", file);
	return;
}


#define SLOCKTIME 10L
#define SLOCKTRIES 5
#define SEQLEN 4

/*******
 *	getseq(snum)	get next sequence number
 *	char *snum;
 *
 *	return codes:  none
 */

getseq(snum)
char *snum;
{
	FILE *fp;
	int n;

	for (n = 0; n < SLOCKTRIES; n++) {
		if (!ulockf( SEQLOCK, SLOCKTIME))
			break;
		sleep(5);
	}

	ASSERT(n < SLOCKTRIES, "CAN NOT GET %s", SEQLOCK);

	if ((fp = fopen(SEQFILE, "r")) != NULL) {
		/* read sequence number file */
		fscanf(fp, "%4d", &n);
		fp = freopen(SEQFILE, "w", fp);
		ASSERT(fp != NULL, "CAN NOT OPEN %s", SEQFILE);
		chmod(SEQFILE, 0666);
	}
	else {
		/* can not read file - create a new one */
		if ((fp = fopen(SEQFILE, "w")) == NULL)
			/* can not write new seqeunce file */
			return(FAIL);
		chmod(SEQFILE, 0666);
		n = 0;
	}

	fprintf(fp, "%s", sprintf(snum, "%04d", ++n));
	fclose(fp);
	rmlock(SEQLOCK);
	return(0);
}
