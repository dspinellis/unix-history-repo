/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)WRITEF.c 1.3 %G%";

#include "h00vars.h"
#include "h01errs.h"

WRITEF(curfile, d1, d2, d3, d4, d5, d6, d7, d8)

	register struct iorec	*curfile;
	FILE			*d1;
	char			*d2;
	int			d3, d4, d5, d6, d7, d8;
{
	if (curfile->funit & FREAD) {
		ERROR(EWRITEIT, curfile->pfname);
		return;
	}
	fprintf(d1, d2, d3, d4, d5, d6, d7, d8);
	if (ferror(curfile->fbuf)) {
		ERROR(EWRITE, curfile->pfname);
		return;
	}
}
