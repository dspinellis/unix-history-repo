/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)TEOF.c 1.1 %G%";

#include "h00vars.h"
#include "h01errs.h"

TEOF(filep)

	register struct iorec	*filep;
{
	if (filep->fblk >= MAXFILES || _actfile[filep->fblk] != filep) {
		ERROR(ENOFILE, 0);
		return;
	}
	if (filep->funit & EOFF)
		return TRUE;
	IOSYNC(filep);
	if (filep->funit & EOFF)
		return TRUE;
	return FALSE;
}
