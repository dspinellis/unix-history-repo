/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)UNIT.c 1.2 6/10/81";

#include "h00vars.h"

struct iorec *
UNIT(curfile)

	register struct iorec	*curfile;
{
	if (curfile->fblk >= MAXFILES || _actfile[curfile->fblk] != curfile) {
		ERROR("Reference to an inactive file\n", 0);
		return;
	}
	if (curfile->funit & FDEF) {
		ERROR("%s: Reference to an inactive file\n", curfile->pfname);
		return;
	}
	return curfile;
}
