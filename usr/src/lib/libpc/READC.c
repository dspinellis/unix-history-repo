/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)READC.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

char
READC(curfile)

	register struct iorec	*curfile;
{
	char			data;

	if (curfile->funit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	IOSYNC(curfile);
	if (curfile->funit & EOFF) {
		ERROR(EPASTEOF, curfile->pfname);
		return;
	}
	curfile->funit |= SYNC;
	return *curfile->fileptr;
}
