/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)itime.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include "dump.h"
#include <sys/file.h>
#include <errno.h>

char *
prdate(d)
	time_t d;
{
	char *p;

	if(d == 0)
		return("the epoch");
	p = ctime(&d);
	p[24] = 0;
	return(p);
}

struct	idates	**idatev = 0;
int	nidates = 0;
int	idates_in = 0;
struct	itime	*ithead = 0;

void	readitimes();
int	getrecord();
int	makeidate();

static void recout();

void
inititimes()
{
	FILE *df;

	if ((df = fopen(increm, "r")) == NULL) {
		if (errno == ENOENT) {
			msg("WARNING: no file `%s'\n", increm);
			return;
		}
		quit("cannot read %s: %s\n", increm, strerror(errno));
		/* NOTREACHED */
	}
	(void) flock(fileno(df), LOCK_SH);
	readitimes(df);
	fclose(df);
}

void
readitimes(df)
	FILE *df;
{
	register	int	i;
	register	struct	itime	*itwalk;

	for (;;) {
		itwalk = (struct itime *)calloc(1, sizeof (struct itime));
		if (getrecord(df, &(itwalk->it_value)) < 0)
			break;
		nidates++;
		itwalk->it_next = ithead;
		ithead = itwalk;
	}

	idates_in = 1;
	/*
	 *	arrayify the list, leaving enough room for the additional
	 *	record that we may have to add to the idate structure
	 */
	idatev = (struct idates **)calloc(nidates + 1,sizeof (struct idates *));
	itwalk = ithead;
	for (i = nidates - 1; i >= 0; i--, itwalk = itwalk->it_next)
		idatev[i] = &itwalk->it_value;
}

void
getitime()
{
	register	struct	idates	*ip;
	register	int	i;
			char	*fname;

	fname = disk;
#ifdef FDEBUG
	msg("Looking for name %s in increm = %s for delta = %c\n",
		fname, increm, incno);
#endif
	spcl.c_ddate = 0;
	lastincno = '0';

	inititimes();
	/*
	 *	Go find the entry with the same name for a lower increment
	 *	and older date
	 */
	ITITERATE(i, ip) {
		if (strncmp(fname, ip->id_name, sizeof (ip->id_name)) != 0)
			continue;
		if (ip->id_incno >= incno)
			continue;
		if (ip->id_ddate <= spcl.c_ddate)
			continue;
		spcl.c_ddate = ip->id_ddate;
		lastincno = ip->id_incno;
	}
}

void
putitime()
{
	FILE		*df;
	register	struct	idates	*itwalk;
	register	int	i;
	int		fd;
	char		*fname;

	if(uflag == 0)
		return;
	if ((df = fopen(increm, "r+")) == NULL)
		quit("cannot rewrite %s: %s\n", increm, strerror(errno));
	fd = fileno(df);
	(void) flock(fd, LOCK_EX);
	fname = disk;
	free(idatev);
	idatev = 0;
	nidates = 0;
	ithead = 0;
	idates_in = 0;
	readitimes(df);
	if (fseek(df, 0L, 0) < 0)
		quit("fseek: %s\n", strerror(errno));
	spcl.c_ddate = 0;
	ITITERATE(i, itwalk) {
		if (strncmp(fname, itwalk->id_name,
				sizeof (itwalk->id_name)) != 0)
			continue;
		if (itwalk->id_incno != incno)
			continue;
		goto found;
	}
	/*
	 *	construct the new upper bound;
	 *	Enough room has been allocated.
	 */
	itwalk = idatev[nidates] =
		(struct idates *)calloc(1, sizeof(struct idates));
	nidates += 1;
  found:
	(void) strncpy(itwalk->id_name, fname, sizeof (itwalk->id_name));
	itwalk->id_incno = incno;
	itwalk->id_ddate = spcl.c_date;

	ITITERATE(i, itwalk) {
		recout(df, itwalk);
	}
	if (fflush(df))
		quit("%s: %s\n", increm, strerror(errno));
	if (ftruncate(fd, ftell(df)))
		quit("ftruncate (%s): %s\n", increm, strerror(errno));
	(void) fclose(df);
	msg("level %c dump on %s\n", incno, prdate(spcl.c_date));
}

static void
recout(file, what)
	FILE	*file;
	struct	idates	*what;
{

	if (fprintf(file, DUMPOUTFMT,
		    what->id_name,
		    what->id_incno,
		    ctime(&what->id_ddate)) < 0)
		quit("%s: %s\n", increm, strerror(errno));
}

int	recno;
int
getrecord(df, idatep)
	FILE	*df;
	struct	idates	*idatep;
{
	char		buf[BUFSIZ];

	recno = 0;
	if ( (fgets(buf, BUFSIZ, df)) != buf)
		return(-1);
	recno++;
	if (makeidate(idatep, buf) < 0)
		msg("Unknown intermediate format in %s, line %d\n",
			increm, recno);

#ifdef FDEBUG
	msg("getrecord: %s %c %s\n",
		idatep->id_name, idatep->id_incno, prdate(idatep->id_ddate));
#endif
	return(0);
}

time_t	unctime();

int
makeidate(ip, buf)
	struct	idates	*ip;
	char	*buf;
{
	char	un_buf[128];

	sscanf(buf, DUMPINFMT, ip->id_name, &ip->id_incno, un_buf);
	ip->id_ddate = unctime(un_buf);
	if (ip->id_ddate < 0)
		return(-1);
	return(0);
}
