/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)UNIT.c 1.1 10/29/80";

#include "h00vars.h"
#include "h01errs.h"

struct iorec *
UNIT(curfile)

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
	return curfile;
}
