/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)SEEK.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

/*
 * Random access routine
 */
SEEK(curfile, loc)

	register struct iorec	*curfile;
	long			loc;
{
	curfile->funit |= SYNC;
	if (fseek(curfile->fbuf, loc, 0) == -1) {
		ERROR(ESEEK, curfile->pfname);
		return;
	}
}
