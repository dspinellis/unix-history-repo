/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READ4.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

READ4(curfile)

	register struct iorec	*curfile;
{
	int			data;

	if (curfile->funit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	UNSYNC(curfile);
	if (fscanf(curfile->fbuf, "%ld", &data) == 0) {
		ERROR(EBADINUM, curfile->pfname);
		return;
	}
	curfile->funit |= SYNC;
	return data;
}
