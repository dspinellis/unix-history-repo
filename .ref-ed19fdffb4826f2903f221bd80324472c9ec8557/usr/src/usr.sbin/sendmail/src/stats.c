# include "sendmail.h"

SCCSID(@(#)stats.c	3.4		%G%);

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

struct statistics	Stat;

poststats(sfile)
	char *sfile;
{
	register int fd;
	struct statistics stat;
	extern long lseek();

	(void) time(&Stat.stat_itime);
	Stat.stat_size = sizeof Stat;

	fd = open(sfile, 2);
	if (fd < 0)
		return;
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
		bmove((char *) &Stat, (char *) &stat, sizeof stat);

	/* write out results */
	(void) lseek(fd, 0L, 0);
	(void) write(fd, (char *) &stat, sizeof stat);
	(void) close(fd);
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
