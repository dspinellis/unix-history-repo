/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)utilities.c	5.2 (Berkeley) 1/3/88";
#endif not lint

#include	<signal.h>
#include	"whoami.h"
#include	"vars.h"
#include	"objfmt.h"
#include	<sys/time.h>
#include	<sys/resource.h>

stats()
{
	struct rusage ru;
	register double l;
	register long count;
#	ifdef PROFILE
#	define	proffile	"/vb/grad/mckusick/px/profile/pcnt.out"
	struct cntrec {
		double	counts[NUMOPS];	/* instruction counts */
		long	runs;		/* number of interpreter runs */
		long	startdate;	/* date profile started */
		long	usrtime;	/* total user time consumed */
		long	systime;	/* total system time consumed */
		double	stmts;		/* number of pascal stmts executed */
	} profdata;
	FILE *datafile;
#	endif PROFILE

	if (_nodump)
		return(0);
	getrusage(RUSAGE_SELF, &ru);
#	ifdef PROFILE
	datafile = fopen(proffile,"r");
	if (datafile == NULL)
		goto skipprof;
	count = fread(&profdata,1,sizeof(profdata),datafile);
	if (count != sizeof(profdata))
		goto skipprof;
	for (count = 0;  count < NUMOPS;  count++)
		profdata.counts[count] += _profcnts[count];
	profdata.runs += 1;
	profdata.stmts += _stcnt;
	profdata.usrtime += ru.ru_utime.tv_sec;
	profdata.systime += ru.ru_stime.tv_sec;
	datafile = freopen(proffile,"w",datafile);
	if (datafile == NULL)
		goto skipprof;
	count = fwrite(&profdata,1,sizeof(profdata),datafile);
	if (count != sizeof(profdata))
		goto skipprof;
	fclose(datafile);
skipprof:
#	endif PROFILE
	fprintf(stderr,
		"\n%1ld statements executed in %d.%02d seconds cpu time.\n",
		_stcnt, ru.ru_utime.tv_sec, ru.ru_utime.tv_usec / 10000);
}

backtrace(type)
	char	*type;
{
	register struct dispsave *mydp;
	register struct blockmark *ap;
	register char *cp;
	register long i, linum;
	union display disp;

	if (_lino <= 0) {
		fputs("Program was not executed.\n",stderr);
		return;
	}
	disp = _display;
	fprintf(stderr, "\n\t%s in \"", type);
	mydp = _dp;
	linum = _lino;
	for (;;) {
		ap = mydp->stp;
		i = linum - (((ap)->entry)->offset & 0177777);
		fprintf(stderr,"%s\"",(ap->entry)->name);
		if (_nodump == FALSE)
			fprintf(stderr,"+%D near line %D.",i,linum);
		fputc('\n',stderr);
		*mydp = (ap)->odisp;
		if (mydp <= &_display.frame[1]){
			_display = disp;
			return;
		}
		mydp = (ap)->dp;
		linum = (ap)->lino;
		fputs("\tCalled by \"",stderr);
	}
}

psexit(code)

	int	code;
{
	if (_pcpcount != 0)
		PMFLUSH(_cntrs, _rtns, _pcpcount);
	if (_mode == PIX) {
		fputs("Execution terminated",stderr);
		if (code)
			fputs(" abnormally",stderr);
		fputc('.',stderr);
		fputc('\n',stderr);
	}
	stats();
	exit(code);
}

/*
 * Routines to field various types of signals
 *
 * catch a library error and generate a backtrace
 */
liberr()
{
	backtrace("Error");
	psexit(2);
}

/*
 * catch an interrupt and generate a backtrace
 */
intr()
{
	signal(SIGINT, intr);
	backtrace("Interrupted");
	psexit(1);
}

/*
 * catch memory faults
 */
memsize()
{
	signal(SIGSEGV, memsize);
	ERROR("Run time stack overflow\n");
}

/*
 * catch random system faults
 */
syserr(signum)
	int signum;
{
	signal(signum, syserr);
	ERROR("Panic: Computational error in interpreter\n");
}
