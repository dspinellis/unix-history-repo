/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PCEXIT.c 1.1 10/30/80";

#include "h00vars.h"

PCEXIT(code)

	int	code;
{
	struct	{
		long	usr_time;
		long	sys_time;
		long	child_usr_time;
		long	child_sys_time;
		} tbuf;
	double l;

	PCLOSE(GLVL);
	PFLUSH();
	if (_stcnt > 0) {
		times(&tbuf);
		l = tbuf.usr_time;
		l = l / HZ;
		fprintf(stderr, "\n%1ld %s %04.2f seconds cpu time.\n",
				_stcnt, "statements executed in", l);
	}
	exit(code);
}
