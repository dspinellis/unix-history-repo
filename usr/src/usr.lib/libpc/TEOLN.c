/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)TEOLN.c 1.4 8/16/82";

#include "h00vars.h"

bool
TEOLN(filep)

	register struct iorec	*filep;
{
	if (filep->fblk >= MAXFILES || _actfile[filep->fblk] != filep ||
	    (filep->funit & FDEF)) {
		ERROR("Reference to an inactive file\n", 0);
		return;
	}
	if (filep->funit & FWRITE) {
		ERROR("%s: eoln is undefined on files open for writing\n",
		    filep->pfname);
		return;
	}
	IOSYNC(filep);
	if (filep->funit & EOFF) {
		ERROR("%s: eoln is undefined when eof is true\n",
		    filep->pfname);
		return;
	}
	if (filep->funit & EOLN)
		return TRUE;
	return FALSE;
}
