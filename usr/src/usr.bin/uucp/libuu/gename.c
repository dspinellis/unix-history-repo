#ifndef lint
static char sccsid[] = "@(#)gename.c	5.5 (Berkeley) %G%";
#endif

#include "uucp.h"

#define SEQLEN 4

/*LINTLIBRARY*/

/*
 *	generate file name
 */
gename(pre, sys, grade, file)
char pre, *sys, grade, *file;
{
	static char sqnum[5];

	getseq(sqnum);
	sprintf(file,"%c.%.*s%c%.*s", pre, SYSNSIZE, sys, grade, SEQLEN, sqnum);
	DEBUG(4, "file - %s\n", file);
}

#define SLOCKTIME 10L
#define SLOCKTRIES 5

/*
 *	get next sequence number
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
	static char *lastchar = NULL;

	if (lastchar == NULL || (snum[SEQLEN-1] = *(lastchar++)) == '\0') {
		for (i = 0; i < SLOCKTRIES; i++) {
			if (!ulockf(SEQLOCK, SLOCKTIME))
				break;
			sleep(5);
		}

		if (i >= SLOCKTRIES) {
			int alphalen;
			logent(SEQLOCK, "CAN NOT LOCK");
			alphalen = strlen(alphabet);
			srand((int)time((time_t *)0));
			for (i=1;i<SEQLEN;i++)
				*snum++ = alphabet[rand() % alphalen];
			lastchar = alphabet;
			*snum = *lastchar++;
			return;
		}

		if ((fd = open(SEQFILE, 2)) >= 0) {
			int alphalen;
			register char	*p;
			char *index();

			alphalen = strlen(alphabet);
			read(fd, snum, SEQLEN);
			/* initialize rand() for possible use */
			srand((int)time((time_t *)0));
			/* increment the penultimate character */
			for (i = SEQLEN - 2; i >= 0; --i) {
				if ((p = index(alphabet, snum[i])) == NULL) {
					p = &alphabet[rand() % alphalen];
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
			for (i = 0; i < SEQLEN; i++)
				snum[i] = alphabet[0];
			if ((fd = creat(SEQFILE, 0666)) < 0)
				return;
		}

		lseek(fd, 0L, 0);
		write(fd, snum, SEQLEN);
		close(fd);
		rmlock(SEQLOCK);
		lastchar = alphabet + 1;
	}
	return;
}
