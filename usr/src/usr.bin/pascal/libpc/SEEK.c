/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)SEEK.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

/*
 * Random access routine
 */
SEEK(curfile, loc)

	register struct iorec	*curfile;
	struct seekptr		*loc;
{
	curfile->funit |= SYNC;
	curfile->funit &= ~(EOFF | EOLN | SPEOLN);
	if (fseek(curfile->fbuf, loc->cnt, 0) == -1) {
		PERROR("Could not seek ", curfile->pfname);
		return;
	}
}
