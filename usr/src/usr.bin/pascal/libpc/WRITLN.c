/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)WRITLN.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

WRITLN(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FREAD) {
		ERROR(EWRITEIT, curfile->pfname);
		return;
	}
	if (++curfile->lcount >= curfile->llimit) {
		ERROR(ELLIMIT, curfile->pfname);
		return;
	}
	fputc('\n', curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		ERROR(EWRITE, curfile->pfname);
		return;
	}
}
