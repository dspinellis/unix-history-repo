/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)stats.c	5.11 (Berkeley) 6/1/90";
#endif /* not lint */

# include "sendmail.h"
# include "mailstats.h"

struct statistics	Stat;

#define ONE_K		1000		/* one thousand (twenty-four?) */
#define KBYTES(x)	(((x) + (ONE_K - 1)) / ONE_K)
/*
**  MARKSTATS -- mark statistics
*/

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
				KBYTES(CurEnv->e_msgsize);
		}
	}
	else
	{
		Stat.stat_nt[to->q_mailer->m_mno]++;
		Stat.stat_bt[to->q_mailer->m_mno] += KBYTES(CurEnv->e_msgsize);
	}
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

poststats(sfile)
	char *sfile;
{
	register int fd;
	struct statistics stat;
	extern off_t lseek();

	if (sfile == NULL)
		return;

	(void) time(&Stat.stat_itime);
	Stat.stat_size = sizeof Stat;

	fd = open(sfile, 2);
	if (fd < 0)
	{
		errno = 0;
		return;
	}
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
}
