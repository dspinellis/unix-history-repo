/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)FNIL.c	1.5 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

char *
FNIL(curfile)

	register struct iorec	*curfile;
{
	if (curfile->fblk >= MAXFILES || _actfile[curfile->fblk] != curfile) {
		ERROR("Reference to an inactive file\n", 0);
	}
	if (curfile->funit & FDEF) {
		ERROR("%s: Reference to an inactive file\n", curfile->pfname);
	}
	if (curfile->funit & FREAD) {
		IOSYNC(curfile);
	}
	return curfile->fileptr;
}
