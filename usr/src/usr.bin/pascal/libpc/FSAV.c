/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)FSAV.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

struct formalrtn *
FSAV(entryaddr, cbn, frtn) 
	long (*entryaddr)();
	long cbn;
	register struct formalrtn *frtn;
{
	frtn->fentryaddr = entryaddr;
	frtn->fbn = cbn;
	blkcpy(&_disply[1], &frtn->fdisp[0],
		frtn->fbn * sizeof(struct display));
	return frtn;
}
