/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)TEOLN.c 1.3 6/10/81";

#include "h00vars.h"

bool
TEOLN(filep)

	register struct iorec	*filep;
{
	if (filep->fblk >= MAXFILES || _actfile[filep->fblk] != filep) {
		ERROR("Reference to an inactive file\n", 0);
		return;
	}
	IOSYNC(filep);
	if (filep->funit & EOLN)
		return TRUE;
	return FALSE;
}
