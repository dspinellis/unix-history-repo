/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PMFLUSH.c 1.1 %G%";

#include "h00vars.h"

PMFLUSH(cntrs, rtns)

	long	cntrs;	/* total number of counters (stmt + routine) */
	long	rtns;	/* number of func and proc counters */
{
	register FILE	*filep;

	_pcpcount[0] = 0426;
	_pcpcount[1] = time();
	_pcpcount[2] = cntrs;
	_pcpcount[3] = rtns;
	filep = fopen(PXPFILE, "w");
	if (filep == NULL)
		goto ioerr;
	fwrite(&_pcpcount[0], cntrs + 1, sizeof(long), filep);
	if (ferror(filep))
		goto ioerr;
	fclose(filep);
	if (!ferror(filep))
		return;
ioerr:
	perror(PXPFILE);
}
