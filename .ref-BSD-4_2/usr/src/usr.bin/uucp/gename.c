#ifndef lint
static char sccsid[] = "@(#)gename.c	5.2 (Berkeley) 7/19/83";
#endif

#include "uucp.h"
#include <sys/types.h>

#define SEQLEN 4

/*******
 *	gename(pre, sys, grade, file)	generate file name
 *	char grade, *sys, pre, *file;
 *
 *	return codes:  none
 */

gename(pre, sys, grade, file)
char pre, *sys, grade, *file;
{
	static char sqnum[5];

	getseq(sqnum);
	sprintf(file, "%c.%.7s%c%.*s", pre, sys, grade, SEQLEN, sqnum);
	DEBUG(4, "file - %s\n", file);
	return;
}


#define SLOCKTIME 10L
#define SLOCKTRIES 5

/*******
 *	getseq(snum)	get next sequence number
 *	char *snum;
 *
 *	return codes:  none
 */

static
getseq(snum)
register char *snum;
{
	/*
	 * the alphabet can be anything, but if it's not in ascii order,
	 * sequence ordering is not preserved
	 */
	char	*alphabet =
	    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	register int i, fd;
	static char *lastchar;

	if (lastchar == NULL || (snum[SEQLEN-1] = *(lastchar++)) == '\0') {
		for (i = 0; i < SLOCKTRIES; i++) {
			if (!ulockf(SEQLOCK, (time_t)SLOCKTIME))
				break;
			sleep(5);
		}

		ASSERT(i < SLOCKTRIES, "CAN NOT GET", "SEQLOCK", 0);

		if ((fd = open(SEQFILE, 2)) >= 0) {
			int alphalen;
			register char	*p;
			char *index();

			alphalen = strlen(alphabet);
			read(fd, snum, SEQLEN);
			/* increment the penultimate character */
			for (i = SEQLEN - 2; i >= 0; --i) {
				if ((p = index(alphabet, snum[i])) == NULL) {
					/* drastic but effective */
					snum[i] = alphabet[alphalen - 1];
					DEBUG(6, "bad seqf: %s\n", snum);
				}
				if (++p < &alphabet[alphalen]) {
					snum[i] = *p;
					break;
				} else		/* carry */
					snum[i] = alphabet[0];	/* continue */
			}
			snum[SEQLEN-1] = alphabet[0];
		} else {
			if ((fd = creat(SEQFILE, 0666)) < 0)
				return(FAIL);
			for (i = 0; i < SEQLEN; i++)
				snum[i] = alphabet[0];
		}

		lseek(fd, 0L, 0);
		write(fd, snum, SEQLEN);
		close(fd);
		rmlock(SEQLOCK);
		lastchar = alphabet + 1;
	}
	return(0);
}
