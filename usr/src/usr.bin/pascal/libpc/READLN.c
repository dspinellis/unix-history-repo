/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)READLN.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

READLN(curfile)

	register struct iorec	*curfile;
{
	do	{
		IOSYNC(curfile);
		curfile->funit |= SYNC;
	} while ((curfile->funit & EOLN) == 0);
}
