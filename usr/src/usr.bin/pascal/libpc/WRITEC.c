/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)WRITEC.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

WRITEC(curfile, d1, d2)

	register struct iorec	*curfile;
	long			d1;
	FILE			*d2;
{
	if (curfile->funit & FREAD) {
		ERROR(EWRITEIT, curfile->pfname);
		return;
	}
	fputc((char)d1, d2);
	if (ferror(curfile->fbuf)) {
		ERROR(EWRITE, curfile->pfname);
		return;
	}
}
