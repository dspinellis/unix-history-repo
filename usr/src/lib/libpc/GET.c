/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)GET.c 1.2 6/10/81";

#include "h00vars.h"

GET(curfile)

	register struct iorec	*curfile;
{
	if (curfile->funit & FWRITE) {
		ERROR("%s: Attempt to read, but open for writing\n",
			curfile->pfname);
		return;
	}
	IOSYNC(curfile);
	if (curfile->funit & EOFF) {
		ERROR("%s: Tried to read past end of file\n", curfile->pfname);
		return;
	}
	curfile->funit |= SYNC;
}
