/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)TELL.c	1.4 (Berkeley) %G%";
#endif /* not lint */

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
