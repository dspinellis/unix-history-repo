/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)PUT.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

PUT(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FREAD) {
		ERROR(EWRITEIT, curfile->pfname);
		return;
	}
	fwrite(curfile->fileptr, (int)curfile->fsize, 1, curfile->fbuf);
	if (ferror(curfile->fbuf)) {
		ERROR(EWRITE, curfile->pfname);
		return;
	}
}
