/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)FRTN.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

FRTN(frtn, save)
	register struct formalrtn *frtn;
	char *save;
{
	blkcpy(save, &_disply[1], frtn->fbn * sizeof(struct display));
}
