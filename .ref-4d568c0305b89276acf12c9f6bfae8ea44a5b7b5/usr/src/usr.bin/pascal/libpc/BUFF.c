/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)BUFF.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

BUFF(amount)

	long		amount;
{
	struct iorec	*curfile;
	static char	sobuf[BUFSIZ];

	curfile = OUTPUT;
	if (amount == 0)
		setbuf(ACTFILE(curfile), 0);
	else if (amount == 2)
		setbuf(ACTFILE(curfile), sobuf);
}
