/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)DEFNAME.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

DEFNAME(filep, name, maxnamlen, datasize)

	register struct iorec	*filep;
	char			*name;
	long			maxnamlen;
	long			datasize;
{
	filep = GETNAME(filep, name, maxnamlen, datasize);
	filep->funit |= FDEF;
}
