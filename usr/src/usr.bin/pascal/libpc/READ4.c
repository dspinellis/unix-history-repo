/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READ4.c 1.3 %G%";

#include "h00vars.h"
#include "h01errs.h"

READ4(curfile)

	register struct iorec	*curfile;
{
	int			data, retval;

	if (curfile->funit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	UNSYNC(curfile);
	retval = fscanf(curfile->fbuf, "%ld", &data);
	if (retval == EOF) {
		ERROR(EPASTEOF, curfile->pfname);
		return;
	}
	if (retval == 0) {
		ERROR(EBADINUM, curfile->pfname);
		return;
	}
	curfile->funit &= ~EOLN;
	curfile->funit |= SYNC;
	return data;
}
