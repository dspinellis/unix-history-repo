/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FNIL.c 1.3 6/10/81";

#include "h00vars.h"

char *
FNIL(curfile)

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
	if (curfile->funit & FREAD) {
		IOSYNC(curfile);
	}
	return curfile->fileptr;
}
