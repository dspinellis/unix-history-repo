/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FLUSH.c 1.2 6/10/81";

#include "h00vars.h"

FLUSH(curfile)

	register struct iorec	*curfile;
{
	if (curfile->fblk >= MAXFILES || _actfile[curfile->fblk] != curfile) {
		ERROR("Reference to an inactive file\n", 0);
		return;
	}
	if (curfile->funit & FWRITE) {
		fflush(curfile->fbuf);
	}
}
