/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FNIL.c 1.2 %G%";

#include "h00vars.h"
#include "h01errs.h"

char *
FNIL(curfile)

	register struct iorec	*curfile;
{
	if (curfile->fblk >= MAXFILES || _actfile[curfile->fblk] != curfile) {
		ERROR(ENOFILE, 0);
		return;
	}
	if (curfile->funit & FDEF) {
		ERROR(EREFINAF, curfile->pfname);
		return;
	}
	if (curfile->funit & FREAD) {
		IOSYNC(curfile);
	}
	return curfile->fileptr;
}
