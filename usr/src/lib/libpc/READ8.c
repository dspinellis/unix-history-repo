/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READ8.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

double
READ8(curfile)

	register struct iorec	*curfile;
{
	double			data;

	if (curfile->funit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	UNSYNC(curfile);
	if (fscanf(curfile->fbuf, "%lf", &data) == 0) {
		ERROR(EBADFNUM, curfile->pfname);
		return;
	}
	curfile->funit |= SYNC;
	return data;
}
