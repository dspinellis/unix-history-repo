/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READ8.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

double
READ8(curfile)

	register struct iorec	*curfile;
{
	double			data;
	int			retval;

	if (curfile->funit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	UNSYNC(curfile);
	retval = fscanf(curfile->fbuf, "%lf", &data);
	if (retval == EOF) {
		ERROR(EPASTEOF, curfile->pfname);
		return;
	}
	if (retval == 0) {
		ERROR(EBADFNUM, curfile->pfname);
		return;
	}
	curfile->funit |= SYNC;
	return data;
}
