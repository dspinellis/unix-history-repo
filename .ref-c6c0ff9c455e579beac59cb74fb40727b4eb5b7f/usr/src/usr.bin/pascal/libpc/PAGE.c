/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PAGE.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

PAGE(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FREAD) {
		ERROR(EWRITEIT, curfile->pfname);
		return;
	}
	fputc('', curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		ERROR(EWRITE, curfile->pfname);
		return;
	}
}
