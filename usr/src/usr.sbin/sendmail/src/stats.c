/*
 * Copyright (c) 1983, 1995 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)stats.c	8.5 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"
# include "mailstats.h"

struct statistics	Stat;
extern long		kbytes();	/* for _bf, _bt */
/*
**  MARKSTATS -- mark statistics
*/

void
markstats(e, to)
	register ENVELOPE *e;
	register ADDRESS *to;
{
	if (to == NULL)
	{
		if (e->e_from.q_mailer != NULL)
		{
			Stat.stat_nf[e->e_from.q_mailer->m_mno]++;
			Stat.stat_bf[e->e_from.q_mailer->m_mno] +=
				KBYTES(e->e_msgsize);
		}
	}
	else
	{
		Stat.stat_nt[to->q_mailer->m_mno]++;
		Stat.stat_bt[to->q_mailer->m_mno] += kbytes(CurEnv->e_msgsize);
	}
	GotStats = TRUE;
}
/*
**  POSTSTATS -- post statistics in the statistics file
**
**	Parameters:
**		sfile -- the name of the statistics file.
**
**	Returns:
**		none.
**
**	Side Effects:
**		merges the Stat structure with the sfile file.
*/

void
poststats(sfile)
	char *sfile;
{
	register int fd;
	struct statistics stat;
	extern off_t lseek();

	if (sfile == NULL || !GotStats)
		return;

	(void) time(&Stat.stat_itime);
	Stat.stat_size = sizeof Stat;

	fd = open(sfile, O_RDWR);
	if (fd < 0)
	{
		errno = 0;
		return;
	}
	(void) lockfile(fd, sfile, NULL, LOCK_EX);
	if (read(fd, (char *) &stat, sizeof stat) == sizeof stat &&
	    stat.stat_size == sizeof stat)
	{
		/* merge current statistics into statfile */
		register int i;

		for (i = 0; i < MAXMAILERS; i++)
		{
			stat.stat_nf[i] += Stat.stat_nf[i];
			stat.stat_bf[i] += Stat.stat_bf[i];
			stat.stat_nt[i] += Stat.stat_nt[i];
			stat.stat_bt[i] += Stat.stat_bt[i];
		}
	}
	else
		bcopy((char *) &Stat, (char *) &stat, sizeof stat);

	/* write out results */
	(void) lseek(fd, (off_t) 0, 0);
	(void) write(fd, (char *) &stat, sizeof stat);
	(void) close(fd);

	/* clear the structure to avoid future disappointment */
	bzero(&Stat, sizeof stat);
	GotStats = FALSE;
}
/*
**  KBYTES -- given a number, returns the number of Kbytes.
**
**	Used in statistics gathering of message sizes to try to avoid
**	wraparound (at least for a while.....)
**
**	Parameters:
**		bytes -- actual number of bytes.
**
**	Returns:
**		number of kbytes.
**
**	Side Effects:
**		none.
**
**	Notes:
**		This function is actually a ceiling function to
**			the nearest K.
**		Honestly folks, floating point might be better.
**			Or perhaps a "statistical" log method.
*/

long
kbytes(bytes)
	long bytes;
{
	return ((bytes + 999) / 1000);
}
