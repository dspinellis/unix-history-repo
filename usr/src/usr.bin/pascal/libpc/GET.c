/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)GET.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

GET(curfile)

	register struct iorec	*curfile;
{
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
}
