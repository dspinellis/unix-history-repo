/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)UNSYNC.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

/*
 * push back last char read to prepare for formatted read
 */
UNSYNC(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FWRITE) {
		ERROR(EREADIT, curfile->pfname);
		return;
	}
	if ((curfile->funit & SYNC) == 0) {
		ungetc(*curfile->fileptr, curfile->fbuf);
	}
}
