/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FLUSH.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

FLUSH(curfile)

	register struct iorec	*curfile;
{
	if (curfile->fblk >= MAXFILES || _actfile[curfile->fblk] != curfile) {
		ERROR(ENOFILE, 0);
		return;
	}
	if (curfile->funit & FWRITE) {
		fflush(curfile->fbuf);
	}
}
