/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)WRITES.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

WRITES(curfile, d1, d2, d3, d4)

	register struct iorec	*curfile;
	int			d1, d2, d3, d4;
{
	if (curfile->funit & FREAD) {
		ERROR(EWRITEIT, curfile->pfname);
		return;
	}
	fwrite(d1, d2, d3, d4);
	if (ferror(curfile->fbuf)) {
		ERROR(EWRITE, curfile->pfname);
		return;
	}
}
