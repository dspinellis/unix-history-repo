/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)UNIT.c	8.1 (Berkeley) %G%";
#endif /* not lint */

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
