/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)FRTN.c	1.6 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

FRTN(frtn, save)
	register struct formalrtn *frtn;
	char *save;
{
	blkcpy(save, &_disply[1], frtn->fbn * sizeof(struct display));
}
