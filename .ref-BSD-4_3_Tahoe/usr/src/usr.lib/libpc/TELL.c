/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)TELL.c 1.3 6/17/81";

#include "h00vars.h"

/*
 * Find current location
 */
struct seekptr
TELL(curfile)

	register struct iorec	*curfile;
{
	struct seekptr loc;

	if ((curfile->funit & FREAD) && (curfile->funit & SYNC) == 0) {
		fseek(curfile->fbuf, -curfile->fsize, 1);
		curfile->funit |= SYNC;
	}
	loc.cnt = ftell(curfile->fbuf);
	return loc;
}
